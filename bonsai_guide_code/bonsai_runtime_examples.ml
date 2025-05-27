open! Core

module My_app = struct
  let component _graph = Bonsai.return (Bonsai_web.Vdom.Node.div [])
end

(* We don't want to unconditionally start an app, contaminating the [bonsai_guide_code]
   app that powers inline examples.*)
module Bonsai_web = struct
  module Start = struct
    let start (_ : Bonsai.graph -> Bonsai_web.Vdom.Node.t Bonsai.t) = ()
  end
end

(* $MDX part-begin=start *)
let () = Bonsai_web.Start.start My_app.component
(* $MDX part-end *)
