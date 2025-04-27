open! Core
open! Bonsai_web

let component (local_ _graph) = Bonsai.return (Vdom.Node.text "hello world")
let () = Bonsai_web.Start.start ~enable_bonsai_telemetry:Enabled component
