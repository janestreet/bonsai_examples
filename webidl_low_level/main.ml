open Js_of_ocaml
module Ll = Webidl_low_level_bindings

let () =
  let window : Ll.window Js.t = Js.Unsafe.global in
  let document = window##.document in
  let body = document##.body in
  let text_node = document##createTextNode ~data:(Js.string "hello world") in
  match Js.Opt.to_option body with
  | Some body ->
    let _ : Ll.node Js.t = body##appendChild ~node:(text_node :> Ll.node Js.t) in
    ()
  | None -> ()
;;
