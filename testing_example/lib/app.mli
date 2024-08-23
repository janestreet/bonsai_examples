open! Core
open! Bonsai_web

val hello_world : local_ Bonsai.graph -> Vdom.Node.t Bonsai.t
val hello_user : string Bonsai.t -> local_ Bonsai.graph -> Vdom.Node.t Bonsai.t
val hello_textbox : local_ Bonsai.graph -> Vdom.Node.t Bonsai.t
