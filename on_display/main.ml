open! Core
open! Bonsai_web
open Bonsai.Let_syntax

let component (local_ graph) =
  let state, set_state =
    Bonsai.state 0 ~sexp_of_model:[%sexp_of: Int.t] ~equal:[%equal: Int.t] graph
  in
  let increment =
    let%arr state and set_state in
    set_state (state + 1)
  in
  let () = Bonsai.Edge.after_display increment graph in
  let%arr state in
  Vdom.Node.textf "Number of frames rendered: %d" state
;;

let () = Bonsai_web.Start.start component
