open! Core
open! Bonsai_web

val component
  :  Bonsai_web_rpgdice_example.Roll_spec.t Or_error.t Bonsai.t
  -> local_ Bonsai.graph
  -> Vdom.Node.t Bonsai.t
