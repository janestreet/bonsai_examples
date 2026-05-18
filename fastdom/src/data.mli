open! Core

module Metric_id : sig
  type t [@@deriving sexp, compare, equal]

  include Stringable with type t := t
  include Comparable.S with type t := t
  include Hashable.Common with type t := t
end

module Metric : sig
  type t =
    { id : string
    ; label : string
    ; value : float
    }
  [@@deriving sexp, compare, equal]
end

val generate_metrics : num_metrics:int -> Metric.t Metric_id.Map.t
