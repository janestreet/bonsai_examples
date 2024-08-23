open! Core
open! Bonsai_web

val current_time : local_ Bonsai.graph -> Vdom.Node.t Bonsai.t
val approx_current_time : local_ Bonsai.graph -> Vdom.Node.t Bonsai.t
val vdom_time_ago : local_ Bonsai.graph -> Vdom.Node.t Bonsai.t
val measure_time : local_ Bonsai.graph -> Vdom.Node.t Bonsai.t
val clock_sleep_demo : local_ Bonsai.graph -> Vdom.Node.t Bonsai.t
val clock_every_demo : local_ Bonsai.graph -> Vdom.Node.t Bonsai.t
