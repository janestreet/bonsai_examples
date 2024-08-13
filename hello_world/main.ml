open! Core
open! Bonsai_web

let component _graph = Bonsai.return (Vdom.Node.text "hello world")
let () = Bonsai_web.Start.start component
