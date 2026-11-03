open! Core
open! Bonsai_web

val current_time : Bonsai.graph @ local -> Vdom.Node.t Bonsai.t
val approx_current_time : Bonsai.graph @ local -> Vdom.Node.t Bonsai.t
val vdom_time_ago : Bonsai.graph @ local -> Vdom.Node.t Bonsai.t
val measure_time : Bonsai.graph @ local -> Vdom.Node.t Bonsai.t
val clock_sleep_demo : Bonsai.graph @ local -> Vdom.Node.t Bonsai.t
val clock_every_demo : Bonsai.graph @ local -> Vdom.Node.t Bonsai.t
