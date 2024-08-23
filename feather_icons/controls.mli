open! Core
open! Import

type t =
  { size : int
  ; stroke_width : float
  ; stroke : [ `Hex of string ]
  ; fill : [ `Hex of string ] option
  }

(** The part on the right where you can control the size/color/etc. of all the icons. *)
val component : local_ Bonsai.graph -> (t * Vdom.Node.t) Bonsai.t
