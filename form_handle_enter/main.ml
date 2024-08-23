open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_automatic_view

module One = struct
  type t = { a : string } [@@deriving sexp, sexp_grammar]
end

module Two = struct
  type t =
    { a : string
    ; b : string
    }
  [@@deriving sexp, sexp_grammar]
end

module Css =
  [%css
  stylesheet
    {|
      .notification {
        color: black;
      }
      |}]

let component (local_ graph) =
  let notifications =
    Bonsai_web_ui_notifications.component (module Unit) ~equal:[%equal: Unit.t] graph
  in
  let rendered_notifications =
    Bonsai_web_ui_notifications.render
      notifications
      ~f:(fun ~close:_ _ (local_ _graph) ->
        Bonsai.return
          (Vdom.Node.div ~attrs:[ Css.notification ] [ Vdom.Node.text "Submitted form" ]))
      graph
  in
  let notify =
    let%arr notifications in
    Bonsai_web_ui_notifications.send_notification
      ~close_after:(Time_ns.Span.of_sec 5.0)
      notifications
      ()
  in
  let form1 =
    Bonsai_web_ui_auto_generated.form
      (module One)
      ~textbox_for_string:()
      graph
      ~allow_updates_when_focused:`Never
  in
  let form2 =
    Bonsai_web_ui_auto_generated.form
      (module One)
      ~textbox_for_string:()
      graph
      ~allow_updates_when_focused:`Never
  in
  let form3 =
    Bonsai_web_ui_auto_generated.form
      (module One)
      ~textbox_for_string:()
      graph
      ~allow_updates_when_focused:`Never
  in
  let form4 =
    Bonsai_web_ui_auto_generated.form
      (module Two)
      ~textbox_for_string:()
      graph
      ~allow_updates_when_focused:`Never
  in
  let form5 =
    Bonsai_web_ui_auto_generated.form
      (module Two)
      ~textbox_for_string:()
      graph
      ~allow_updates_when_focused:`Never
  in
  let form6 =
    Bonsai_web_ui_auto_generated.form
      (module Two)
      ~textbox_for_string:()
      graph
      ~allow_updates_when_focused:`Never
  in
  let%arr rendered_notifications
  and notify
  and form1
  and form2
  and form3
  and form4
  and form5
  and form6 in
  Vdom.Node.div
    [ rendered_notifications
    ; Vdom.Node.text "handle_enter = true"
    ; Form.view_as_vdom
        ~on_submit:
          (Form.Submit.create ~handle_enter:true ~f:(fun _ -> Effect.ignore_m notify) ())
        form1
    ; Vdom.Node.hr ()
    ; Vdom.Node.text "handle_enter = true, button = None"
    ; Form.view_as_vdom
        ~on_submit:
          (Form.Submit.create
             ~handle_enter:true
             ~button:None
             ~f:(fun _ -> Effect.ignore_m notify)
             ())
        form2
    ; Vdom.Node.hr ()
    ; Vdom.Node.text "handle_enter = false, button = None"
    ; Form.view_as_vdom
        ~on_submit:
          (Form.Submit.create
             ~handle_enter:false
             ~button:None
             ~f:(fun _ -> Effect.ignore_m notify)
             ())
        form3
    ; Vdom.Node.hr ()
    ; Vdom.Node.text "handle_enter = true"
    ; Form.view_as_vdom
        ~on_submit:
          (Form.Submit.create ~handle_enter:true ~f:(fun _ -> Effect.ignore_m notify) ())
        form4
    ; Vdom.Node.hr ()
    ; Vdom.Node.text "handle_enter = true, button = None"
    ; Form.view_as_vdom
        ~on_submit:
          (Form.Submit.create
             ~handle_enter:true
             ~button:None
             ~f:(fun _ -> Effect.ignore_m notify)
             ())
        form5
    ; Vdom.Node.hr ()
    ; Vdom.Node.text "handle_enter = false, button = None"
    ; Form.view_as_vdom
        ~on_submit:
          (Form.Submit.create
             ~handle_enter:false
             ~button:None
             ~f:(fun _ -> Effect.ignore_m notify)
             ())
        form6
    ]
;;

let () = Bonsai_web.Start.start component
