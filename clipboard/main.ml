open! Core
open! Bonsai_web
open Js_of_ocaml
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_automatic_view

let component (local_ graph) =
  let form = Form.Elements.Textbox.string ~allow_updates_when_focused:`Never () graph in
  let copy_button =
    let%arr form in
    Vdom.Node.button
      ~attrs:
        [ Vdom.Attr.on_click (fun _ ->
            match%bind.Effect
              Js_clipboard.Asynchronous.copy_text
                (Js.string (Form.value_or_default form ~default:""))
            with
            | Ok () -> Effect.Ignore
            | Error err ->
              Effect.print_s [%message "Failed to copy to clipboard." (err : Error.t)])
        ]
      [ Vdom.Node.text "copy to clipboard" ]
  in
  (* NOTE: This button copies from clipboard in a deprecated and non-recommended way. Use
     the above button implementation as a reference if you'd like clipboard copying
     behavior in your app. *)
  let legacy_copy_button =
    let%arr form in
    Vdom.Node.button
      ~attrs:
        [ Vdom.Attr.on_click (fun _ ->
            Ui_effect.Expert.handle
              (Effect.ignore_m
                 (Js_clipboard.Asynchronous.copy_text
                    (Js.string (Form.value_or_default form ~default:""))))
              ~on_exn:(fun exn -> Exn.reraise exn "Unhandled exception raised in effect");
            Effect.Ignore)
        ]
      [ Vdom.Node.text "copy to clipboard (legacy behaviour)" ]
  in
  let paste_button =
    let%arr form in
    Vdom.Node.button
      ~attrs:
        [ Vdom.Attr.on_click (fun _ ->
            let%bind.Effect st = Js_clipboard.Asynchronous.read_text in
            match st with
            | Ok st -> Form.set form (Js.to_string st)
            | Error error ->
              Effect.print_s
                [%message "Error reading from clipboard" ~_:(error : Error.t)])
        ]
      [ Vdom.Node.text "paste from clipboard" ]
  in
  let copy_blob_button =
    Vdom.Node.button
      ~attrs:
        [ Vdom.Attr.on_click (fun _ ->
            match%bind.Effect
              Js_clipboard.Asynchronous.copy_blob
                [ Js_of_ocaml.File.blob_from_string
                    ~contentType:"text/plain"
                    "https://janestreet.com/"
                ; Js_of_ocaml.File.blob_from_string
                    ~contentType:"text/html"
                    [%string
                      "<div><a href=\"https://janestreet.com/\">Jane Street</a></div>"]
                ]
            with
            | Ok () -> Effect.Ignore
            | Error err ->
              Effect.print_s
                [%message "Failed to copy blob to clipboard." (err : Error.t)])
        ]
      [ Vdom.Node.text "copy blob to clipboard (paste into rich editor to see)" ]
  in
  let%arr form and copy_button and legacy_copy_button and paste_button in
  Vdom.Node.div
    [ Form.view_as_vdom form
    ; copy_button
    ; legacy_copy_button
    ; paste_button
    ; copy_blob_button
    ]
;;

let () = Start.start component ~enable_bonsai_telemetry:Enabled
