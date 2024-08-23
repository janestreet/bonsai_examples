open! Core
open! Bonsai_web

val component
  :  send_message:(string -> unit Effect.t) Bonsai.t
  -> local_ Bonsai.graph
  -> Vdom.Node.t Bonsai.t
