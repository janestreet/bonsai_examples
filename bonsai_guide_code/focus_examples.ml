open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

(* $MDX part-begin=effect_focus_api *)
let effect_focus_demo graph =
  let focus = Effect.Focus.on_effect () graph in
  let%arr { attr; focus; blur } = focus in
  Vdom.Node.div
    [ Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> focus) ]
        [ Vdom.Node.text "focus input" ]
    ; Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> blur) ]
        [ Vdom.Node.text "blur input" ]
    ; Vdom.Node.input ~attrs:[ attr ] ()
    ]
;;

(* $MDX part-end *)

let () = Util.run effect_focus_demo ~id:"effect_focus_api"

(* $MDX part-begin=effect_focus_onactivate *)
let effect_focus_onactivate graph =
  let visible, set_visible = Bonsai.state false graph in
  let subview =
    match%sub visible with
    | false -> return Vdom.Node.none
    | true ->
      let autofocus = Effect.Focus.on_activate () graph in
      let%arr autofocus = autofocus in
      Vdom.Node.input ~attrs:[ autofocus ] ()
  in
  let%arr visible = visible
  and set_visible = set_visible
  and subview = subview in
  Vdom.Node.div
    [ Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> set_visible (not visible)) ]
        [ Vdom.Node.text "toggle" ]
    ; subview
    ]
;;

(* $MDX part-end *)

let () = Util.run effect_focus_onactivate ~id:"effect_focus_onactivate"

(* $MDX part-begin=autofocus *)
module Toplayer = Bonsai_web_ui_toplayer

let autofocus graph =
  let { Toplayer.Controls.open_; _ } =
    Toplayer.Modal.create
      ~content:(fun ~close:_ _ ->
        return (Vdom.Node.input ~attrs:[ Vdom.Attr.autofocus true ] ()))
      graph
  in
  let%arr open_ = open_ in
  Vdom.Node.div
    [ Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> open_) ]
        [ Vdom.Node.text "open modal" ]
    ]
;;

(* $MDX part-end *)

let () = Util.run autofocus ~id:"autofocus"
