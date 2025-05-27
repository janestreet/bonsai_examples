open! Core
open Bonsai_web
open Bonsai.Let_syntax
open Js_of_ocaml

(* $MDX part-begin=noop_hook *)

module Noop_hook = struct
  module T = struct
    module Input = struct
      (* Hooks don't run in [Bonsai_web_test] tests; instead, we display an attribute with
         the input name, and a sexp of the input value. *)
      type t = unit [@@deriving sexp_of]

      (* Multiple hooks of the same "class" get merged by Vdom.
         You'll need to implement receiving multiple sets of inputs.*)
      let combine () () = ()
    end

    module State = Unit

    let init (_input : Input.t) (_element : Dom_html.element Js.t) =
      (* [init] will run once the hook is attached to a DOM node. The DOM node might
         not yet be mounted! This means that you can e.g. attach event listeners,
         but not query the DOM / layout / size / etc.

         [init]'s main job is to create a [State.t], which often has some mutable fields.
      *)
      ()
    ;;

    let on_mount (_input : Input.t) (_state : State.t) (_element : Dom_html.element Js.t) =
      (* [on_mount] will run once the DOM node has been connected. There are 3 options:
         - [`Do_nothing] won't run an [on_mount]
         - [`Schedule_immediately_after_this_dom_patch_completes] will synchronously run
           [on_mount] immediately after the patch completes. This only works if the patch
           is done by Bonsai; calling [Vdom.Node.to_dom] outside of a hook or widget won't
           run [on_mount]
         - [`Schedule_animation_frame] will run [on_mount] via [requestAnimationFrame].
           This is not recommended, because it adds frame delays, and forces asynchronicity.
      *)
      ()
    ;;

    let on_mount = `Schedule_immediately_after_this_dom_patch_completes on_mount

    let update
      ~old_input:_
      ~new_input:_
      (_state : State.t)
      (_element : Dom_html.element Js.t)
      =
      (* [update] is called on **every patch** to the DOM node. This is usually where you
         would handle changes to inputs. Most hooks should explicitly check for input
         equality to avoid unnecessary work. *)
      ()
    ;;

    let destroy (_input : Input.t) (_state : State.t) (_element : Dom_html.element Js.t) =
      (* [destroy] will run once the hook is removed from the DOM. This could mean that:
         - The element was removed
         - The hook was removed
         - A diff moved the hook from one element to another*)
      ()
    ;;
  end

  include T

  (* It is important that the hook functor is called exactly once for each "class" of
     hooks. *)
  include Vdom.Attr.Hooks.Make (T)
end

(* Typically, this would be a function that takes inputs, and passes them to
   [Hook_module.create]. *)
let noop_hook : Vdom.Attr.t = Vdom.Attr.create_hook "noop" (Noop_hook.create ())
(* $MDX part-end *)

let () = Fn.ignore noop_hook

(* $MDX part-begin=trivial_widget *)
open Js_of_ocaml

module Trivial_widget = struct
  (* Hooks force you to define the type of DOM node you're using. If you're not sure, or
     it's dynamic, you can use [Dom_html.element]. *)
  type dom = Dom_html.divElement

  let name = "trivial_widget"

  module Input = Bool
  module State = Int

  (* Just a helper function.*)
  let redraw input state element =
    let display = if input then state else state * -1 in
    element##.innerHTML := Js.string (sprintf "%d" display)
  ;;

  (* [create] will run on patch when the vdom is being converted to a DOM node.
     Note that there are NO SAFEGUARDS against script injection, bad inputs, etc.
     You are responsible for following security best practices. *)
  let create input =
    let state = Random.int 100 in
    let element = Dom_html.createDiv Dom_html.document in
    redraw input state element;
    state, element
  ;;

  (* [update] will run on every subsequent patch. *)
  let update ~prev_input ~input ~state ~element =
    match [%equal: Input.t] prev_input input with
    | true -> state, element
    | false ->
      redraw input state element;
      state, element
  ;;

  (* [destroy] will run when the widget is removed from the DOM. Do any cleanup here;
     e.g. removing event listeners, undoing side effects, etc. *)
  let destroy ~prev_input:_ ~state:_ ~element:_ = ()

  (* As with hooks, widgets don't run in [Bonsai_web_test]. But widgets give us a bit
     more control over how their input is displayed in tests.
     You could also just pass [let to_vdom_for_testing = `Sexp_of_input].*)
  let to_vdom_for_testing =
    `Custom
      (fun input ->
        Vdom.Node.create "trivial_widget" [ Vdom.Node.textf "Inverted: %b" input ])
  ;;
end

(* As with [Vdom.Attr.Hooks.Make], it is important that [Vdom.Node.widget_of_module] is
   called exactly once for each "class" of widgets. *)
let (trivial_widget : bool -> Vdom.Node.t) =
  unstage (Vdom.Node.widget_of_module (module Trivial_widget))
;;

let app (local_ graph) =
  let invert, toggle_invert = Bonsai.toggle ~default_model:false graph in
  let%arr invert and toggle_invert in
  let widget = trivial_widget invert in
  (* Note that each usage of widget has a separate internal state! *)
  {%html|
    <div>
      <button on_click=%{fun _ -> toggle_invert}>Invert</button>
      %{widget} %{widget} %{widget}
    </div>
  |}
;;

(* $MDX part-end *)

let () = Util.run app ~id:"trivial_widget"

(* $MDX part-begin=widget_using_diff_patch *)
open Js_of_ocaml

module Widget_using_diff_patch = struct
  type dom = Dom_html.element

  let name = "widget_using_diff_patch"

  module Input = struct
    type t =
      { set_num_patches : int -> unit Effect.t
      ; vdom : Vdom.Node.t
      }

    let sexp_of_t = sexp_of_opaque
  end

  module State = struct
    type t = { num_patches : int } [@@deriving sexp_of]
  end

  let create { Input.vdom; _ } =
    let initial_element = Vdom.Node.to_dom vdom in
    let state = { State.num_patches = 0 } in
    state, initial_element
  ;;

  let update ~(prev_input : Input.t) ~(input : Input.t) ~(state : State.t) ~element =
    let num_patches = state.num_patches + 1 in
    (* You might need to use [Effect.Expert.handle_non_dom_event_exn] from within hooks
       and widgets to interop with Bonsai. Do not do so anywhere else!
       Also, this example dispatches an effect on every patch, which is a bad idea. *)
    Effect.Expert.handle_non_dom_event_exn (input.set_num_patches num_patches);
    match phys_equal input.vdom prev_input.vdom with
    | true -> { State.num_patches }, element
    | false ->
      let patch = Vdom.Node.Patch.create ~previous:prev_input.vdom ~current:input.vdom in
      let new_element = Vdom.Node.Patch.apply patch element in
      { num_patches }, new_element
  ;;

  let destroy ~prev_input:_ ~state:_ ~element:_ = ()
  let to_vdom_for_testing = `Sexp_of_input
end

let widget_using_diff_patch =
  unstage (Vdom.Node.widget_of_module (module Widget_using_diff_patch))
;;

let app (local_ graph) =
  let num_clicks, update_num_clicks = Bonsai.state' 0 graph in
  let num_patches, set_num_patches = Bonsai.state 0 graph in
  let view =
    let%arr num_patches and num_clicks in
    {%html|
      <div>
        <p>Clicks: %{num_clicks#Int}</p>
        <p>Patches: %{num_patches#Int}</p>
      </div>
    |}
  in
  let%arr view and set_num_patches and update_num_clicks in
  (* NOTE: If we used this widget more than once, the separate widget instances would be
     independently setting the same state. Usually, you'll want to account for this by
     maintaining state as a keyed map, rather than a single value. *)
  let widget =
    widget_using_diff_patch { Widget_using_diff_patch.Input.vdom = view; set_num_patches }
  in
  {%html|
    <div>
      <button on_click=%{fun _ -> update_num_clicks (fun x -> x + 1)}>
        Click me
      </button>
      %{widget}
    </div>
  |}
;;

(* $MDX part-end *)

let () = Util.run app ~id:"widget_using_diff_patch"
