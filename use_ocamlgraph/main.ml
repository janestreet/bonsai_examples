open! Core
open! Bonsai_web
module _ = Graph

let component (local_ _graph) = Bonsai.return (Vdom.Node.text "hello world")
let () = Bonsai_web.Start.start component ~enable_bonsai_telemetry:Enabled
