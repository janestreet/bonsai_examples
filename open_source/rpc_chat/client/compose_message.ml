open! Core
open! Bonsai_web

let build_result ~send_message ~textbox_content ~set_textbox_content =
  let submit_and_then_clear =
    Vdom.Effect.Many [ send_message textbox_content; set_textbox_content "" ]
  in
  let on_ret =
    let is_key_ret key =
      String.equal
        "Enter"
        (key##.code
         |> Js_of_ocaml.Js.Optdef.to_option
         |> Option.value_exn
         |> Js_of_ocaml.Js.to_string)
    in
    Vdom.Attr.on_keypress (fun key ->
      if is_key_ret key then submit_and_then_clear else Vdom.Effect.Ignore)
  in
  let on_input = Vdom.Attr.on_input (fun _ -> set_textbox_content) in
  let value = Vdom.Attr.string_property "value" textbox_content in
  let text_input =
    (Vdom.Node.input_deprecated [@alert "-deprecated"])
      ~attrs:[ on_ret; on_input; value ]
      [ Vdom.Node.text "submit" ]
  in
  let submit_button =
    Vdom_input_widgets.Button.simple
      ~merge_behavior:Legacy_dont_merge
      "send"
      ~on_click:(fun _ -> submit_and_then_clear)
  in
  Vdom.Node.div ~attrs:[ Vdom.Attr.id "compose" ] [ text_input; submit_button ]
;;

let component ~send_message (local_ graph) =
  let open Bonsai.Let_syntax in
  let textbox_content, set_textbox_content =
    Bonsai.state_machine0
      graph
      ~sexp_of_model:[%sexp_of: String.t]
      ~equal:[%equal: String.t]
      ~sexp_of_action:[%sexp_of: String.t]
      ~default_model:""
      ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) _ new_state -> new_state)
  in
  let%arr send_message and textbox_content and set_textbox_content in
  build_result ~send_message ~textbox_content ~set_textbox_content
;;
