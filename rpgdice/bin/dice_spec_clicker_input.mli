open! Core
open! Bonsai_web

val component
  :  local_ Bonsai.graph
  -> (Bonsai_web_rpgdice_example.Roll_spec.t * Vdom.Node.t) Bonsai.t
