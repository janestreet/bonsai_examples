open! Core
open! Async_kernel
open! Bonsai_web
open Bonsai.Let_syntax

module type Conv = sig
  type t

  val of_string : string -> t
  val to_string_hum : t -> string
end

let component (type t) (module Conv : Conv with type t = t) ~default_model graph =
  let text, set_text =
    Bonsai.state
      default_model
      ~sexp_of_model:[%sexp_of: String.t]
      ~equal:[%equal: String.t]
      graph
  in
  let%arr text = text
  and set_text = set_text in
  let conv = Or_error.try_with (fun () -> Conv.of_string text) in
  let textbox =
    Vdom.Node.input
      ~attrs:
        [ Vdom.Attr.type_ "text"
        ; Vdom.Attr.on_input (fun _ -> set_text)
        ; Vdom.Attr.value text
        ; (match conv with
           | Ok _ -> Vdom.Attr.empty
           | Error _ -> Vdom.Attr.class_ "invalid")
        ]
      ()
  in
  let conv_display =
    let attr, text =
      match conv with
      | Error err -> Vdom.Attr.class_ "invalid", Error.to_string_hum err
      | Ok spec -> Vdom.Attr.empty, Conv.to_string_hum spec
    in
    Vdom.Node.pre ~attrs:[ attr ] [ Vdom.Node.text text ]
  in
  conv, Vdom.Node.div [ textbox; conv_display ]
;;
