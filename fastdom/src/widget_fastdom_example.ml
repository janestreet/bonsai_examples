open! Core
open! Bonsai_web
open Bonsai.Let_syntax
open Skyline_tokens_v2
module Dom_html = Js_of_ocaml.Dom_html
module Js = Js_of_ocaml.Js

module Style =
  [%css
  stylesheet
    {|
      .dashboard {
        font-family: system-ui, sans-serif;
        padding: 20px;
        max-width: 1200px;
        margin: 0 auto;
      }

      .metric_row {
        display: flex;
        align-items: center;
        padding: 4px 8px;
        border-bottom: 1px solid %{Colors.Border.default#Css_gen.Color};
      }

      .metric_row:hover {
        background: %{Colors.Background.two#Css_gen.Color};
      }

      .label {
        width: 120px;
        font-size: 13px;
        color: %{Colors.Text.secondary#Css_gen.Color};
        flex-shrink: 0;
      }

      .bar_container {
        flex: 1;
        height: 20px;
        background: %{Colors.Background.three#Css_gen.Color};
        border-radius: 4px;
        overflow: hidden;
        margin: 0 12px;
      }

      .bar_fill {
        height: 100%;
        background: %{Colors.Background.primary#Css_gen.Color};
        border-radius: 4px;
        transition: none;
      }

      .value_text {
        width: 60px;
        text-align: right;
        font-size: 13px;
        font-variant-numeric: tabular-nums;
        color: %{Colors.Text.default#Css_gen.Color};
      }

      .metrics_container {
        border: 1px solid %{Colors.Border.default#Css_gen.Color};
        border-radius: 8px;
        overflow: hidden;
        max-height: 80vh;
        overflow-y: auto;
      }

      .header {
        display: flex;
        align-items: center;
        padding: 8px;
        background: %{Colors.Background.two#Css_gen.Color};
        border-bottom: 2px solid %{Colors.Border.default#Css_gen.Color};
        font-weight: 600;
        font-size: 13px;
        color: %{Colors.Text.default#Css_gen.Color};
      }
    |}]

module Bar_widget_good = struct
  type dom = Dom_html.element

  let name = "bar-widget"

  module Input = struct
    type t = float [@@deriving sexp_of]
  end

  module State = struct
    type t = unit [@@deriving sexp_of]
  end

  let create input_value =
    let vdom =
      {%html|<div %{Style.bar_fill} data-bar-value=%{input_value#Float}></div>|}
    in
    let element = vdom |> Vdom.Node.to_dom in
    let () = element##.style##.width := Js.string [%string "%{input_value#Float}px"] in
    (), element
  ;;

  let update ~prev_input:_ ~input ~state:() ~(element : dom Js.t) =
    let () =
      Fastdom.read_then_write
        ~read:(fun () ->
          Js.Opt.map element##.parentNode (fun parent ->
            (Js.Unsafe.coerce parent)##.clientWidth)
          |> Js.Opt.to_option)
        ~write:(fun client_width ->
          Option.iter client_width ~f:(fun container_width ->
            let width_px =
              Float.iround_nearest_exn (input *. Float.of_int container_width)
            in
            element##.style##.width := Js.string [%string "%{width_px#Int}px"]))
    in
    (), element
  ;;

  let destroy ~prev_input:_ ~state:_ ~element:_ = ()
  let to_vdom_for_testing = `Sexp_of_input
end

module Bar_widget = struct
  type dom = Dom_html.element

  let name = "bar-widget"

  module Input = struct
    type t = float [@@deriving sexp_of]
  end

  module State = struct
    type t = unit [@@deriving sexp_of]
  end

  let create input_value =
    let vdom =
      {%html|<div %{Style.bar_fill} data-bar-value=%{input_value#Float}></div>|}
    in
    let element = vdom |> Vdom.Node.to_dom in
    let () = element##.style##.width := Js.string [%string "%{input_value#Float}px"] in
    (), element
  ;;

  let update ~prev_input:_ ~input ~state:() ~element =
    let () =
      Js.Opt.iter element##.parentNode (fun parent ->
        Js.Opt.iter (Dom_html.CoerceTo.element parent) (fun parent ->
          (* READ: forces reflow if there was a previous WRITE *)
          let container_width = parent##.clientWidth in
          (* WRITE: invalidates layout for the next READ *)
          let width_px =
            Float.iround_nearest_exn (input *. Float.of_int container_width)
          in
          element##.style##.width := Js.string [%string "%{width_px#Int}px"]))
    in
    (), element
  ;;

  let destroy ~prev_input:_ ~state:_ ~element:_ = ()
  let to_vdom_for_testing = `Sexp_of_input
end

let bar_widget_bad = Vdom.Node.widget_of_module (module Bar_widget) |> Staged.unstage

let bar_widget_good =
  Vdom.Node.widget_of_module (module Bar_widget_good) |> Staged.unstage
;;

let fiddle_with_data metrics =
  Map.map metrics ~f:(fun (metric : Data.Metric.t) ->
    match Random.int_incl 0 2 with
    | 0 ->
      let delta = Random.float_range (-0.05) 0.05 in
      let value = Float.clamp_exn ~min:0.0 ~max:1.0 (metric.value +. delta) in
      { metric with value }
    | _ -> metric)
;;

let component (local_ graph) =
  let data, inject_action =
    Bonsai.state_machine
      ~default_model:(Data.generate_metrics ~num_metrics:1_000)
      ~apply_action:(fun _ctx model () -> fiddle_with_data model)
      graph
  in
  let time_between, set_time_between = Bonsai.state' [] graph in
  let get_time = Bonsai.Clock.get_current_time graph in
  let () =
    Bonsai.Edge.after_display
      (let%arr set_time_between and get_time in
       let%bind.Effect curr_time = get_time in
       set_time_between (fun prev -> curr_time :: prev))
      graph
  in
  let avg_time_display =
    let%arr time_between in
    let avg_time =
      match time_between with
      | [] -> Time_ns.Span.of_ns 0.
      | hd :: tl ->
        let _, acc =
          List.fold tl ~init:(hd, 0.) ~f:(fun (prev, acc) curr ->
            curr, acc +. (Time_ns.diff prev curr |> Time_ns.Span.to_ns))
        in
        acc /. (List.length time_between |> Int.to_float) |> Time_ns.Span.of_ns
    in
    {%html|<div>Avg time between frames: %{avg_time#Time_ns.Span}</div>|}
  in
  let use_which, set_use_which = Bonsai.state `Bad graph in
  Bonsai.Clock.every
    ~when_to_start_next_effect:`Every_multiple_of_period_non_blocking
    (Bonsai.return (Time_ns.Span.of_int_ms 200))
    (let%arr inject_action in
     inject_action ())
    graph;
  let%arr data
  and use_which
  and set_use_which
  and avg_time_display
  and set_time_between in
  let bar_widget =
    match use_which with
    | `Bad -> bar_widget_bad
    | `Good -> bar_widget_good
  in
  let toggle =
    let s =
      match use_which with
      | `Bad -> true
      | `Good -> false
    in
    let set_s = function
      | true -> set_use_which `Bad
      | false -> set_use_which `Good
    in
    let set_s input =
      let%bind.Effect () = set_s input in
      set_time_between (fun _ -> [])
    in
    Skyline_switch_input_v2.content ~state:(s, set_s) ()
  in
  let label =
    let which =
      match use_which with
      | `Bad -> "bad"
      | `Good -> "good"
    in
    {%html|<span>Currently using <b>#{which}</b></span>|}
  in
  let metric_rows =
    Map.fold data ~init:[] ~f:(fun ~key:_ ~data:metric acc ->
      let _data_attr = Vdom.Attr.create "data-bar-value" (sprintf "%.6f" metric.value) in
      let row =
        {%html|
          <div %{Style.metric_row}>
            <span %{Style.label}>%{metric.label#String}</span>
            <div %{Style.bar_container}>%{bar_widget metric.value}</div>
            <span %{Style.value_text}> %{sprintf "%.1f%%" (metric.value *. 100.0)#String} </span>
          </div>
        |}
      in
      row :: acc)
    |> List.rev
  in
  {%html|
    <div %{Style.dashboard}>
      <h1
        style="
          font-size: 24px;
          font-weight: 600;
          margin-bottom: 16px;
          color: %{Colors.Text.default#Css_gen.Color};
        "
      >
        #{" Metrics Dashboard "}
      </h1>
      <div
        style="display: flex; justify-content: space-between; margin-bottom: 8px"
      >
        <Skyline_field_v2.view
          style="
            display: flex;
            justify-content: space-between;
            gap: 8px;
            flex-direction: row-reverse;
          "
          ~label:%{Skyline_field_v2.Label.content [ label]}
          >%{toggle}</>%{avg_time_display}
      </div>
      <div %{Style.metrics_container}>
        <div %{Style.header}>
          <span %{Style.label}>#{"Metric"}</span
          ><span style="flex: 1; margin: 0 12px">#{"Value"}</span
          ><span %{Style.value_text}>#{"%"}</span>
        </div>
        *{metric_rows}
      </div>
    </div>
  |}
;;
