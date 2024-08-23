open! Core
open! Bonsai_web

type t =
  { tag : string
  ; reset : unit Effect.t
  }

val component : local_ Bonsai.graph -> t Bonsai.t
