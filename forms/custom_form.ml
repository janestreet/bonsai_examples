open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_manual_view

let form (local_ graph) =
  let state, set_state =
    Bonsai.state_machine ~default_model:"" graph ~apply_action:(fun _ _ new_state ->
      String.capitalize new_state)
  in
  let%arr state and set_state in
  let view =
    Vdom_input_widgets.Entry.text
      ~value:(Some state)
      ~on_input:(function
        | None -> set_state ""
        | Some s -> set_state s)
      ()
  in
  { Form.value = Ok state; set = set_state; view }
;;

let component graph =
  let%arr { view; _ } = form graph in
  Vdom.Node.div
    [ Vdom.Node.h1 [ Vdom.Node.text "Custom form" ]
    ; Vdom.Node.p
        [ Vdom.Node.text
            "this form automatically capitalizes the first character of the input"
        ]
    ; view
    ]
;;
