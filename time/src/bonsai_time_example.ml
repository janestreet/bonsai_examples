open! Core
open Bonsai_web
open Bonsai.Let_syntax

(* $MDX part-begin=untestable-clock-component *)
let untestable_clock =
  let now = Incr.Clock.watch_now Incr.clock |> Bonsai.Incr.to_value in
  return (now >>| Time_ns.to_string_utc >>| Vdom.Node.text)
;;

(* $MDX part-end *)

let () = ignore untestable_clock

(* $MDX part-begin=testable-clock-component *)
let testable_bonsai_clock (local_ graph) =
  let now = Bonsai.Incr.with_clock ~f:Bonsai.Time_source.watch_now graph in
  now >>| Time_ns.to_string_utc >>| Vdom.Node.text
;;

(* $MDX part-end *)

let time_now (local_ graph) =
  let state, set_state = Bonsai.state Int63.zero graph in
  Bonsai.Clock.every
    ~when_to_start_next_effect:`Every_multiple_of_period_blocking
    ~trigger_on_activate:false
    (Bonsai.return (Time_ns.Span.of_sec 1.))
    (let%arr set_state in
     let%bind.Effect time =
       Effect.of_thunk (fun () -> Time_now.nanosecond_counter_for_timing ())
     in
     set_state time)
    graph;
  let%arr state in
  let span = Time_ns.Span.of_int63_ns state in
  {%html|
    <div>
      This counter should show the # of seconds since the page opened:
      %{span#Time_ns.Span}
    </div>
  |}
;;

let component (local_ graph) =
  let%arr bonsai_time_source = testable_bonsai_clock graph
  and time_now = time_now graph in
  {%html|
    <div>
      <h2>Bonsai Time Source</h2>
      <div>%{bonsai_time_source}</div>
      <h2>Clock.every + Time_now</h2>
      <div>%{time_now}</div>
    </div>
  |}
;;
