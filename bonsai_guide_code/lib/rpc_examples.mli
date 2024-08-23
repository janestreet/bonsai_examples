open! Core
open! Bonsai_web
open Async_rpc_kernel

val custom_connector : Rpc_effect.Where_to_connect.Custom.t -> Rpc_effect.Connector.t
val double_implementation : Async_rpc_kernel_private.Connection.t Rpc.Implementation.t
val double_number_app : local_ Bonsai.graph -> Vdom.Node.t Bonsai.t
val current_time_app : local_ Bonsai.graph -> Vdom.Node.t Bonsai.t
