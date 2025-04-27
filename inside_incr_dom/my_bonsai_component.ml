open! Core
open Bonsai_web
open Bonsai.Let_syntax

module Time_source_inside_of_apply_action = struct
  let component (local_ graph) =
    let model, inject =
      Bonsai.state_machine
        ~default_model:`Resting
        ~apply_action:(fun ctx model action ->
          let inject = Bonsai.Apply_action_context.inject ctx in
          let time_source = Bonsai.Apply_action_context.time_source ctx in
          let schedule_event = Bonsai.Apply_action_context.schedule_event ctx in
          match model, action with
          | `Resting, `Poke ->
            schedule_event
              (let%bind.Effect () =
                 Bonsai.Time_source.sleep time_source (Time_ns.Span.of_sec 1.0)
               in
               inject `Sleep);
            `Pending
          | `Pending, `Sleep -> `Resting
          | `Resting, `Sleep | `Pending, `Poke -> model)
        graph
    in
    let%map model
    and inject
    and theme = View.Theme.current graph in
    let text =
      match model with
      | `Resting -> "resting"
      | `Pending -> "pending"
    in
    View.vbox
      ~attrs:[ {%css|width: fit-content;|} ]
      [ Vdom.Node.text text; View.button theme ~on_click:(inject `Poke) "poke" ]
  ;;
end

include
  (val Bonsai_web.To_incr_dom.convert (fun (_ : unit Bonsai.t) (local_ graph) ->
         let counters = Bonsai_web_counters_example.application graph in
         let () =
           Bonsai.Edge.lifecycle
             ~on_activate:(Bonsai.return (Bonsai.Effect.print_s [%message "hi!"]))
             graph
         in
         let () =
           Bonsai.Clock.every
             ~when_to_start_next_effect:`Every_multiple_of_period_blocking
             (Bonsai.return (Time_ns.Span.of_sec 1.0))
             (Bonsai.return (Bonsai.Effect.print_s [%message "tick"]))
             graph
         in
         let wait_after_display = Bonsai.Edge.wait_after_display graph in
         let print_button =
           let%arr wait_after_display in
           Vdom.Node.button
             ~attrs:
               [ Vdom.Attr.on_click (fun _ ->
                   let%bind.Effect () = wait_after_display in
                   Effect.print_s [%message "after display"])
               ]
             [ Vdom.Node.text "Print after display" ]
         in
         let%arr counters
         and print_button
         and delayer = Time_source_inside_of_apply_action.component graph in
         Vdom.Node.div [ counters; print_button; delayer ]))
