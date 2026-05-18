open! Core
module Metric_id = String

module Metric = struct
  type t =
    { id : string
    ; label : string
    ; value : float
    }
  [@@deriving sexp, compare, equal]
end

let generate_metrics ~num_metrics =
  List.init num_metrics ~f:(fun i ->
    let id = sprintf "metric-%d" i in
    let label = sprintf "Metric %d" i in
    let value = Random.float 1.0 in
    id, { Metric.id; label; value })
  |> Metric_id.Map.of_alist_exn
;;
