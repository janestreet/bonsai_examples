open! Core
open! Async_kernel
open! Bonsai_web
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_manual_view

(* $MDX part-begin=form_textbox_value *)
let textbox_value (local_ graph) =
  let textbox =
    Form.Elements.Textbox.string ~allow_updates_when_focused:`Always () graph
  in
  let%arr textbox in
  let view = Form.view textbox in
  let value = Form.value textbox in
  Vdom.Node.div
    [ View.hbox ~gap:(`Px 5) [ Vdom.Node.text "my textbox"; view ]
    ; Vdom.Node.sexp_for_debugging ([%sexp_of: string Or_error.t] value)
    ]
;;

(* $MDX part-end *)
let () = Util.run textbox_value ~id:"form_textbox_value"

(* $MDX part-begin=textbox_with_label *)
let labelled_textbox (label : string Bonsai.t) (local_ graph)
  : (string, Vdom.Node.t) Form.t Bonsai.t
  =
  let textbox =
    Form.Elements.Textbox.string ~allow_updates_when_focused:`Always () graph
  in
  let%arr textbox and label in
  Form.map_view textbox ~f:(fun view ->
    View.hbox ~gap:(`Px 5) [ Vdom.Node.text label; view ])
;;

(* $MDX part-end *)
let () =
  Util.run
    (fun (local_ graph) ->
      let%arr textbox = labelled_textbox (Bonsai.return "Cool textbox:") graph in
      Form.view textbox)
    ~id:"textbox_with_label"
;;

(* $MDX part-begin=textbox_with_error *)
let display_error error =
  View.text ~attrs:[ {%css|color: red;|} ] [%string "⚠  %{Error.to_string_hum error}"]
;;

let int_with_error_display (local_ graph) =
  let textbox = Form.Elements.Textbox.int ~allow_updates_when_focused:`Always () graph in
  let%arr textbox in
  Form.map_view textbox ~f:(fun view ->
    let error_display =
      match Form.value textbox with
      | Ok _ -> Vdom.Node.none
      | Error error -> display_error error
    in
    View.hbox ~gap:(`Px 5) [ view; error_display ])
;;

(* $MDX part-end *)

let () =
  Util.run
    (fun (local_ graph) ->
      let%arr int_with_error_display = int_with_error_display graph in
      Form.view int_with_error_display)
    ~id:"textbox_with_error"
;;

(* $MDX part-begin=with_submit_button *)
let with_submit_button (form : ('a, 'view) Form.t) ~(on_submit : 'a -> unit Effect.t) =
  let submit_button ~extra_attrs =
    Vdom.Node.button
      ~attrs:({%css|width: max-content;|} :: extra_attrs)
      [ Vdom.Node.text "Submit!" ]
  in
  Form.map_view form ~f:(fun view ->
    let submit_button =
      match Form.value form with
      | Ok value ->
        submit_button ~extra_attrs:[ Vdom.Attr.on_click (fun _ -> on_submit value) ]
      | Error _ -> submit_button ~extra_attrs:[ Vdom.Attr.disabled ]
    in
    View.vbox ~gap:(`Px 5) [ view; submit_button ])
;;

(* $MDX part-end *)

(* $MDX part-begin=textbox_with_submit *)
let textbox_with_submit (local_ graph) =
  let textbox = int_with_error_display graph in
  let%arr textbox in
  with_submit_button textbox ~on_submit:(fun value -> Effect.alert (Int.to_string value))
;;

(* $MDX part-end *)

let () =
  Util.run
    (fun (local_ graph) ->
      let%arr textbox_with_submit = textbox_with_submit graph in
      Form.view textbox_with_submit)
    ~id:"textbox_with_submit"
;;

(* $MDX part-begin=form_set *)
let form_set (local_ graph) =
  let textbox =
    Form.Elements.Textbox.string ~allow_updates_when_focused:`Always () graph
  in
  let%arr textbox in
  Vdom.Node.div
    [ View.hbox ~gap:(`Px 5) [ Vdom.Node.text "my textbox"; Form.view textbox ]
    ; Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> Form.set textbox "hello world") ]
        [ Vdom.Node.text "click me" ]
    ]
;;

(* $MDX part-end *)

let () = Util.run form_set ~id:"form_set"

(* $MDX part-begin=form_both *)
let big_tuple_form (local_ graph)
  : ((int * float) * (string * bool), Vdom.Node.t) Form.t Bonsai.t
  =
  let int_and_float =
    let%arr int = Form.Elements.Textbox.int ~allow_updates_when_focused:`Always () graph
    and float =
      Form.Elements.Textbox.float ~allow_updates_when_focused:`Always () graph
    in
    Form.both int float
  in
  let string_and_bool =
    let%arr string =
      Form.Elements.Textbox.string ~allow_updates_when_focused:`Always () graph
    and bool = Form.Elements.Checkbox.bool ~default:false () graph in
    Form.both string bool
  in
  let%arr int_and_float and string_and_bool in
  Form.both int_and_float string_and_bool
  |> Form.map_view ~f:(fun ((int_view, float_view), (string_view, bool_view)) ->
    View.vbox
      [ View.hbox
          ~gap:(`Px 5)
          [ Vdom.Node.text "an int: "; int_view; Vdom.Node.text "a float: "; float_view ]
      ; View.hbox
          ~gap:(`Px 5)
          [ Vdom.Node.text "a string: "
          ; string_view
          ; Vdom.Node.text "a bool: "
          ; bool_view
          ]
      ])
;;

(* $MDX part-end *)

let () =
  Util.run
    (fun (local_ graph) ->
      let%arr big_tuple_form = big_tuple_form graph in
      View.vbox
        [ Form.view big_tuple_form
        ; Vdom.Node.sexp_for_debugging
            ([%sexp_of: ((int * float) * (string * bool)) Or_error.t]
               (Form.value big_tuple_form))
        ])
    ~id:"form_both"
;;

(* $MDX part-begin=record_form_type *)

type t =
  { some_string : string
  ; an_int : int
  ; on_or_off : bool
  }
[@@deriving typed_fields, sexp_of]

(* $MDX part-end *)

(* $MDX part-begin=record_form *)
module Record = struct
  type t =
    { some_string : string
    ; an_int : int
    ; on_or_off : bool
    }
  [@@deriving typed_fields, sexp_of]

  let form : local_ Bonsai.graph -> (t, Vdom.Node.t) Form.t Bonsai.t =
    Form.Typed.Record.make
      (module struct
        (* Reimport the module that typed_fields just derived *)
        module Typed_field = Typed_field

        (* The type of the view for each field's subform *)
        type field_view = Vdom.Node.t

        (* The type of the view for the entire form *)
        type resulting_view = Vdom.Node.t

        (* This type definition is boilerplate and will become unneccessary once OCaml
           gets polymorphic parameters. *)
        type form_of_field_fn =
          { f : 'a. 'a Typed_field.t @ local -> ('a, field_view) Form.t Bonsai.t }

        (* Provide a form computation for each field in the record *)
        let form_for_field
          : type a.
            a Typed_field.t -> local_ Bonsai.graph -> (a, field_view) Form.t Bonsai.t
          =
          fun typed_field (local_ graph) ->
          match typed_field with
          | Some_string ->
            Form.Elements.Textbox.string ~allow_updates_when_focused:`Always () graph
          | An_int ->
            Form.Elements.Number.int
              ~default:0
              ~step:1
              ~allow_updates_when_focused:`Always
              ()
              graph
          | On_or_off -> Form.Elements.Checkbox.bool ~default:false () graph
        ;;

        (* Combine the views of each subform into the view for the resulting form *)
        let finalize_view { f } (local_ _graph) : resulting_view Bonsai.t =
          let%arr some_string_form = f Some_string
          and an_int_form = f An_int
          and on_or_off_form = f On_or_off in
          View.vbox
            [ View.hbox [ View.text "Some string:"; Form.view some_string_form ]
            ; View.hbox [ View.text "An int:"; Form.view an_int_form ]
            ; View.hbox [ View.text "On or off:"; Form.view on_or_off_form ]
            ]
        ;;
      end)
  ;;
end

(* $MDX part-end *)

(* $MDX part-begin=variant_form *)
module Variant = struct
  type t =
    | A
    | B of int
    | C of string
  [@@deriving typed_variants, sexp_of]

  let form : local_ Bonsai.graph -> (t, Vdom.Node.t) Form.t Bonsai.t =
    Form.Typed.Variant.make
      (module struct
        (* Reimport the module that typed_variants just derived *)
        module Typed_variant = Typed_variant

        (* The type of the view for the clause picker subform *)
        type picker_view = Vdom.Node.t

        (* The type of the view for each clause's subform *)
        type variant_view = Vdom.Node.t

        (* The type of the view for the resulting form *)
        type resulting_view = Vdom.Node.t

        (* Provide a form computation for selecting which clause *)
        let form_for_picker =
          Form.Elements.Dropdown.list
            (module Typed_variant.Packed)
            (Bonsai.return Typed_variant.Packed.all)
            ~equal:Typed_variant.Packed.equal
        ;;

        (* Provide a form computation for constructing the clause's arguments *)
        let form_for_variant
          : type a.
            a Typed_variant.t -> local_ Bonsai.graph -> (a, variant_view) Form.t Bonsai.t
          =
          fun typed_variant (local_ graph) ->
          match typed_variant with
          | A ->
            Form.return () |> Form.map_view ~f:(fun () -> Vdom.Node.none) |> Bonsai.return
          | B -> Form.Elements.Textbox.int ~allow_updates_when_focused:`Always () graph
          | C -> Form.Elements.Textbox.string ~allow_updates_when_focused:`Always () graph
        ;;

        (* Combine the views of the picker and corresponding subform into the view for the
           resulting form *)
        let finalize_view picker clause_form (local_ _graph) =
          let clause_view =
            match%arr clause_form with
            | Ok (_which_clause, clause_form) -> Form.view clause_form
            | Error err -> Vdom.Node.sexp_for_debugging [%message (err : Error.t)]
          in
          let%arr clause_view and picker in
          View.vbox [ View.hbox [ View.text "Pick a constructor:"; picker ]; clause_view ]
        ;;
      end)
  ;;
end

(* $MDX part-end *)

let () =
  Util.run
    (fun graph ->
      let%arr record_form = Record.form graph in
      Form.view record_form)
    ~id:"record_form"
;;

(* $MDX part-begin=record_and_variant_form *)
let view_record_and_variant_form : local_ Bonsai.graph -> Vdom.Node.t Bonsai.t =
  fun graph ->
  let form =
    let%arr record_form = Record.form graph
    and variant_form = Variant.form graph in
    Form.both record_form variant_form
  in
  let%arr form in
  let record_view, variant_view = Form.view form in
  View.vbox
    [ Vdom.Node.h4 [ View.text "Record" ]
    ; record_view
    ; Vdom.Node.hr ()
    ; Vdom.Node.h4 [ View.text "Variant" ]
    ; variant_view
    ; Vdom.Node.hr ()
    ; Vdom.Node.sexp_for_debugging
        ([%sexp_of: (Record.t * Variant.t) Or_error.t] (Form.value form))
    ]
;;

(* $MDX part-end *)

let () = Util.run view_record_and_variant_form ~id:"record_and_variant_form"

(* $MDX part-begin=int_textbox *)
let int (local_ graph) : (int, Vdom.Node.t) Form.t Bonsai.t =
  let form = Form.Elements.Textbox.string ~allow_updates_when_focused:`Always () graph in
  let%arr form in
  Form.project form ~parse_exn:Int.of_string ~unparse:Int.to_string
;;

(* $MDX part-end *)

let () =
  Util.run
    (fun graph ->
      let%arr form = int graph in
      Vdom.Node.div
        [ Form.view form
        ; Vdom.Node.sexp_for_debugging ([%sexp_of: int Or_error.t] (Form.value form))
        ])
    ~id:"int_textbox"
;;
