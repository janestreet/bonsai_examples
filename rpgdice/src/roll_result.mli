open! Core

module Die : sig
  type t =
    { num_faces : int
    ; result : int
    }
  [@@deriving bin_io, compare ~localize, equal ~localize, sexp]
end

type t =
  { dice : Die.t list
  ; const : int
  }
[@@deriving bin_io, compare ~localize, equal ~localize, sexp]

val to_int : t -> int
val to_string_hum : t -> string
