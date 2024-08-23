open! Core
open! Async_kernel
open! Bonsai_web

val counter
  :  step:int Bonsai.t
  -> local_ Bonsai.graph
  -> Vdom.Node.t Bonsai.t * int Bonsai.t

val counter_ui : local_ Bonsai.graph -> Vdom.Node.t Bonsai.t
