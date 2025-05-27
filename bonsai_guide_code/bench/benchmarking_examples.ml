open! Core
open Bonsai.For_open

let my_app _ = Bonsai.return ""

(* $MDX part-begin=app_startup *)
let app_startup_bench : Bonsai_bench.t =
  Bonsai_bench.create_for_startup ~name:"app startup" my_app
;;

(* $MDX part-end *)

let list_of_things ~size:_ _ = Bonsai.return ""

(* $MDX part-begin=list_of_things_startup *)
let list_of_things_bench : Bonsai_bench.t list =
  let create ~size =
    Bonsai_bench.create_for_startup
      ~name:[%string "List of %{size#Int} things"]
      (list_of_things ~size:(Bonsai.return size))
  in
  [ create ~size:1
  ; create ~size:5
  ; create ~size:10
  ; create ~size:100
  ; create ~size:1_000
  ]
;;

(* $MDX part-end *)
open Bonsai.Let_syntax

(* $MDX part-begin=state_interaction_bench *)
open Bonsai_bench

let state_bench : Bonsai_bench.t =
  Bonsai_bench.create
    ~name:"Bonsai.state"
    ~component:(fun (local_ graph) ->
      let state, set_state =
        Bonsai.state 0 ~sexp_of_model:[%sexp_of: Int.t] ~equal:[%equal: Int.t] graph
      in
      let%arr state and set_state in
      state, set_state)
    ~get_inject:(fun (_, inject) -> inject)
    Interaction.(many_with_recomputes [ inject 1; reset_model ])
;;

(* $MDX part-end *)

(* $MDX part-begin=running_benchmarks *)

let () =
  let quota = Core_bench_js.Quota.Span (Time_float.Span.of_sec 1.0) in
  Bonsai_bench.benchmark
    ~run_config:(Core_bench_js.Run_config.create () ~quota)
    ([ app_startup_bench ] @ list_of_things_bench @ [ state_bench ])
;;

(* $MDX part-end *)

(* $MDX part-begin=profile *)

let () = Bonsai_bench.profile [ app_startup_bench; state_bench ]
(* $MDX part-end *)

let[@inline never] foo a =
  let to_add =
    (* Deliberately slow. *)
    List.init 10_000 ~f:(fun _ -> Int.to_float a *. Random.float 1.12)
    |> List.sort ~compare:Float.compare
    |> List.sort ~compare:(Comparable.reverse Float.compare)
    |> List.reduce_exn ~f:( +. )
  in
  a * Float.to_int to_add
;;

let[@inline never] bar b = b * 3

(* $MDX part-begin=functions_to_compare *)
let f1 a b =
  let%arr a and b in
  foo a + bar b
;;

let f2 a b =
  let foo_a =
    let%arr a in
    foo a
  in
  let bar_b =
    let%arr b in
    bar b
  in
  let%arr foo_a and bar_b in
  foo_a + bar_b
;;

(* $MDX part-end *)

(* $MDX part-begin=startup_comparison *)

let computations =
  let wrap f a_b (local_ _graph) =
    let%sub a, b = a_b in
    f a b
  in
  [ "f1", wrap f1; "f2", wrap f2 ]
;;

let startup_inputs = [ "same", (2, 2); "different", (1, 2) ]
let () = Bonsai_bench.benchmark_compare_startup ~computations startup_inputs
(* $MDX part-end *)

(* $MDX part-begin=interaction_comparison *)
let scenarios =
  let update_inputs ~a_update_numerator ~a_update_denominator input =
    List.init 100 ~f:(fun i ->
      Bonsai_bench.Interaction.update_input
        input
        ~f:
          (if i mod a_update_denominator < a_update_numerator
           then Tuple2.map_fst ~f:(fun x -> x + 1)
           else Tuple2.map_snd ~f:(fun x -> x + 1)))
    |> Bonsai_bench.Interaction.many_with_recomputes
  in
  [ { Bonsai_bench.Scenario.test_name = "only a changes"
    ; initial = 2, 2
    ; interaction = update_inputs ~a_update_numerator:1 ~a_update_denominator:1
    }
  ; { Bonsai_bench.Scenario.test_name = "mostly a changes"
    ; initial = 2, 2
    ; interaction = update_inputs ~a_update_numerator:9 ~a_update_denominator:10
    }
  ; { Bonsai_bench.Scenario.test_name = "a and b change equally"
    ; initial = 2, 2
    ; interaction = update_inputs ~a_update_numerator:1 ~a_update_denominator:2
    }
  ; { Bonsai_bench.Scenario.test_name = "mostly b changes"
    ; initial = 2, 2
    ; interaction = update_inputs ~a_update_numerator:1 ~a_update_denominator:10
    }
  ; { Bonsai_bench.Scenario.test_name = "only b changes"
    ; initial = 2, 2
    ; interaction = update_inputs ~a_update_numerator:0 ~a_update_denominator:1
    }
  ]
;;

let () =
  Bonsai_bench.benchmark_compare_interactions
    ~get_inject:(fun _ _ -> Effect.Ignore)
    ~computations
    scenarios
;;
(* $MDX part-end *)
