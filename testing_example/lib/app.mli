open! Core
open! Bonsai_web

val hello_world : Bonsai.graph @ local -> Vdom.Node.t Bonsai.t
val hello_user : string Bonsai.t -> Bonsai.graph @ local -> Vdom.Node.t Bonsai.t
val hello_textbox : Bonsai.graph @ local -> Vdom.Node.t Bonsai.t
