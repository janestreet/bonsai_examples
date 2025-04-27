open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

let compute_magic_number a b = a * b

(* $MDX part-begin=typical_f *)
let a_typical_function (input : int Bonsai.t) (local_ graph) =
  (* Declare your state *)
  let num_input_changes, incr_num_input_changes =
    Bonsai.state_machine
      ~default_model:0
      ~apply_action:(fun _ model () -> model + 1)
      graph
  in
  let (logs : string Bonsai.t), (write_log_line : (string -> unit Effect.t) Bonsai.t) =
    Bonsai.state_machine
      ~default_model:""
      ~apply_action:(fun _ logs new_log_line ->
        match logs with
        | "" -> new_log_line
        | _ -> logs ^ ", " ^ new_log_line)
      graph
  in
  (* Build intermediate computations *)
  let magic_number =
    let%arr input and num_input_changes in
    compute_magic_number input num_input_changes
  in
  let log_current_magic_number =
    let%arr magic_number
    and write_log_line
    and now = Bonsai.Clock.get_current_time graph in
    let%bind.Effect now in
    write_log_line [%string "%{Time_ns.to_string_utc now} : %{magic_number#Int}"]
  in
  let on_change : (int -> unit Effect.t) Bonsai.t =
    let%arr incr_num_input_changes in
    fun _new_val -> incr_num_input_changes ()
  in
  (* Declare lifecycle and edge-triggered effects.
     Most code won't need these, but they're not uncommon. *)
  Bonsai.Edge.on_change ~equal:[%equal: int] input ~callback:on_change graph;
  (* Compute main output of your function. This could also be a [match%sub]. *)
  let%arr logs and magic_number and log_current_magic_number in
  View.vbox
    [ Vdom.Node.h1
        [ Vdom.Node.text [%string "Today's Magic Number: %{magic_number#Int}"] ]
    ; Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> log_current_magic_number) ]
        [ Vdom.Node.text "Save it!" ]
    ; Vdom.Node.div [ Vdom.Node.text [%string "Saved Magic Numbers: %{logs}"] ]
    ]
;;

(* $MDX part-end *)

let () =
  Util.run
    (fun (local_ graph) ->
      let input =
        let%arr approx_now =
          Bonsai.Clock.approx_now ~tick_every:(Time_ns.Span.of_sec 2.) graph
        in
        Time_ns.to_span_since_epoch approx_now |> Time_ns.Span.to_int_sec
      in
      a_typical_function input graph)
    ~id:"typical_f"
;;

(* $MDX part-begin=state_with_resetter *)
let state_with_resetter ~default_value (local_ graph)
  : int Bonsai.t * (int -> unit Effect.t) Bonsai.t * unit Effect.t Bonsai.t
  =
  let state_and_setter, reset =
    Bonsai.with_model_resetter
      ~f:(fun (local_ graph) ->
        let state, set_state = Bonsai.state default_value graph in
        Bonsai.both state set_state)
      graph
  in
  let%sub (state : int Bonsai.t), (set_state : (int -> unit Effect.t) Bonsai.t) =
    (state_and_setter : (int * (int -> unit Effect.t)) Bonsai.t)
  in
  state, set_state, reset
;;

(* $MDX part-end *)

let () = ignore state_with_resetter
