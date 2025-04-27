open! Core
open! Bonsai_web

val run
  :  ?custom_connector:(Rpc_effect.Where_to_connect.Custom.t -> Rpc_effect.Connector.t)
  -> id:string
  -> (local_ Bonsai.graph -> Vdom.Node.t Bonsai.t)
  -> unit

val run_vdom_val : Vdom.Node.t Bonsai.t -> id:string -> unit
val run_vdom : Vdom.Node.t -> id:string -> unit
val no_hash_fallback_must_run_last : unit -> unit
