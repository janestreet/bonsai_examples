open Js_of_ocaml
module Exp = Browser_expert

let () =
  let window : Exp.window Js.t = Js.Unsafe.global in
  let document = window##.document in
  let body = document##.body in
  let text_node = document##createTextNode ~data:(Js.string "hello world") in
  match Js.Opt.to_option body with
  | Some body ->
    let _ : Exp.node Js.t = body##appendChild ~node:(text_node :> Exp.node Js.t) in
    ()
  | None -> ()
;;
