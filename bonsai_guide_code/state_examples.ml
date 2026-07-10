open! Core
open! Async_kernel
open! Bonsai_web
open Bonsai.Let_syntax

let counter_ui (local_ graph) =
  let count, set_count = Bonsai.state 0 graph in
  let%arr count and set_count in
  (* view-construction logic *)
  Vdom.Node.div
    [ Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> set_count (count - 1)) ]
        [ Vdom.Node.text "-1" ]
    ; Vdom.Node.text [%string "Counter value: %{count#Int}"]
    ; Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> set_count (count + 1)) ]
        [ Vdom.Node.text "+1" ]
    ]
;;

let counter_state_machine_with_input ~(step : int Bonsai.t) (local_ graph) =
  let count, inject =
    Bonsai.state_machine_with_input
      ~default_model:0
      ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) input model action ->
        match input with
        | Bonsai.Computation_status.Inactive ->
          (* This state machine is inactive, so it can't access the current value of
             [input]. Just keep the original model. *)
          model
        | Active step ->
          (match action with
           | `Increment -> model + step
           | `Decrement -> model - step))
      step
      graph
  in
  let view =
    let%arr step and count and inject in
    Vdom.Node.div
      [ Vdom.Node.button
          ~attrs:[ Vdom.Attr.on_click (fun _ -> inject `Decrement) ]
          [ Vdom.Node.text [%string "-%{step#Int}"] ]
      ; Vdom.Node.text [%string "Counter value: %{count#Int}"]
      ; Vdom.Node.button
          ~attrs:[ Vdom.Attr.on_click (fun _ -> inject `Increment) ]
          [ Vdom.Node.text [%string "+%{step#Int}"] ]
      ]
  in
  view, count
;;

let counter = counter_state_machine_with_input
