open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Gauge = Bonsai_web_ui_gauge
open Vdom

module Styles =
  [%css
  stylesheet
    {|
      html {
        font-family: "Open Sans", "Noto Color Emoji", sans-serif;
      }
      .paper {
        box-shadow: 0 0 8px rgba(0, 0, 0, 0.2);
        padding: 12px;
        border-radius: 4px;
        margin: 8px;
        max-width: fit-content;
        text-align: center;
      }

      .row {
        display: flex;
        flex-direction: row;
        align-items: center;
        align-content: center;
      }

      .column {
        display: flex;
        flex-direction: column;
        align-items: center;
        align-content: center;
      }

      pre {
        font-family: "Courier New", monospace;
        background-color: #fcfcfc;
        border: 1px solid #e7e7e7;
        text-align: left;
        margin: 4px;
        padding: 4px;
        max-height: 50vh;
        overflow-y: auto;
      }
      |}]

let colors =
  [| "#ff355e"
   ; "#fd5b78"
   ; "#ff6037"
   ; "#ff9966"
   ; "#ff9933"
   ; "#ffcc33"
   ; "#ffff66"
   ; "#ccff00"
   ; "#66ff66"
   ; "#aaf0d1"
   ; "#50bfe6"
   ; "#ff6eff"
   ; "#ee34d2"
   ; "#ff00cc"
  |]
;;

let radius = 30.

let ticker (local_ graph) =
  let percentage, increase =
    Bonsai.state_machine0
      graph
      ~sexp_of_model:[%sexp_of: Int.t]
      ~equal:[%equal: Int.t]
      ~sexp_of_action:[%sexp_of: Unit.t]
      ~default_model:0
      ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) model () ->
        (model + 1) % 101)
  in
  let color_index, increment =
    Bonsai.state_machine0
      graph
      ~sexp_of_model:[%sexp_of: Int.t]
      ~equal:[%equal: Int.t]
      ~sexp_of_action:[%sexp_of: Unit.t]
      ~default_model:0
      ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) prev () ->
        prev + (1 % Array.length colors))
  in
  let%sub () =
    let effect =
      let%arr increase and increment in
      Effect.Many [ increase (); increment () ]
    in
    Bonsai.Clock.every
      ~when_to_start_next_effect:`Every_multiple_of_period_blocking
      ~trigger_on_activate:false
      (Time_ns.Span.of_sec 0.1)
      effect
      graph;
    Bonsai.return ()
  in
  let%arr percentage and color_index in
  Percent.of_percentage (Int.to_float percentage), color_index
;;

let component (local_ graph) =
  let%sub percentage, color_index = ticker graph in
  let gauge1 =
    let%arr percentage in
    Gauge.create ~radius percentage
  in
  let gauge2 =
    let percent_to_color p =
      let open Float in
      let p = Percent.to_percentage p in
      if p < 30.0
      then Tailwind_colors.amber600
      else if p < 60.0
      then Tailwind_colors.amber500
      else if p < 90.0
      then Tailwind_colors.red600
      else Tailwind_colors.red500
    in
    let%arr percentage in
    Gauge.create ~percent_to_color ~radius percentage
  in
  let gauge3 =
    let percent_to_color =
      let%arr color_index in
      let color = Array.get colors (color_index % Array.length colors) in
      Fn.const (`Hex color)
    in
    let%arr percent_to_color and percentage in
    Gauge.create ~percent_to_color ~radius percentage
  in
  let%arr gauge1 and gauge2 and gauge3 in
  Node.div
    ~attrs:[ Styles.column ]
    [ Node.strong [ Node.text "Gauges" ]
    ; Node.div
        ~attrs:[ Styles.row ]
        [ Node.div ~attrs:[ Styles.paper ] [ gauge1 ]
        ; Node.div ~attrs:[ Styles.paper ] [ gauge2 ]
        ; Node.div ~attrs:[ Styles.paper ] [ gauge3 ]
        ]
    ]
;;

let () = Bonsai_web.Start.start component
