open! Core
open Import
open! Gen_js_api

let start_time = Time_ns.of_string_with_utc_offset "2020-10-06 09:30:00-04"
let end_time = Time_ns.of_string_with_utc_offset "2020-10-08 16:00:00-04"
let zone = Lazy.force Timezone.local

(* these times will span multiple days, but will not contain overnights *)
let times : Time_ns.t list =
  let start_ofday = Time_ns.Ofday.create ~hr:9 ~min:30 () in
  let stop_ofday = Time_ns.Ofday.create ~hr:16 ~min:00 () in
  List.range'
    ~stop:`inclusive
    start_time
    end_time
    ~compare:Time_ns.compare
    ~stride:(fun t -> Time_ns.add t (Time_ns.Span.of_min 1.))
  |> List.filter ~f:(fun t ->
    let ofday = Time_ns.to_ofday t ~zone in
    Time_ns.Ofday.( >= ) ofday start_ofday && Time_ns.Ofday.( <= ) ofday stop_ofday)
;;

(* a brownian motion *)
let raw_data =
  let last = ref 0. in
  Array.of_list_map times ~f:(fun time ->
    let noise = Random.float 1. -. 0.5 in
    last := !last +. noise;
    time, [| !last |])
;;

let axes ~value_formatter ~axis_label_formatter =
  let axis_options =
    Dygraph.Options.Axis_options.create
      ?valueFormatter:value_formatter
      ?axisLabelFormatter:axis_label_formatter
      ()
  in
  Dygraph.Options.Axes.create ~x:axis_options ()
;;

let options ~title ?value_formatter ?axis_label_formatter () =
  Dygraph.Options.create
    ()
    ~title
    ~width:800
    ~axes:(axes ~value_formatter ~axis_label_formatter)
    ~highlightSeriesOpts:
      (Dygraph.Options.Highlight_series_options.create () ~strokeWidth:1.5)
;;

let app (local_ graph) =
  let x_label = "time" in
  let y_labels = [ "brownian motion" ] in
  let make_graph
    ~name
    ~title
    ~data
    ?value_formatter
    ?axis_label_formatter
    ()
    (local_ graph)
    =
    let options = options ~title ?value_formatter ?axis_label_formatter () in
    let%sub { graph_view; _ } =
      Dygraph.With_bonsai.create
        ~key:(Bonsai.return name)
        ~x_label:(Bonsai.return x_label)
        ~per_series_info:
          (y_labels |> Dygraph.Per_series_info.create_all_visible |> Bonsai.return)
        ~options:(Bonsai.return options)
        ~data:(Bonsai.return data)
        ~with_graph:(fun graph ->
          Js.Unsafe.set Dom_html.window (sprintf "g_%s" name) graph)
        ()
        graph
    in
    graph_view
  in
  let hide_overnights_graph =
    let { Dygraph.X_axis_mapping.time_to_x_value
        ; x_value_to_time = _
        ; value_formatter
        ; axis_label_formatter
        }
      =
      Dygraph_jane.X_axis_mapping.only_display_market_hours
        ()
        ~start_time
        ~end_time
        ~view_zone:zone
      |> Or_error.ok_exn
    in
    let data =
      Array.map raw_data ~f:(fun (time, values) -> time_to_x_value time, values)
      |> Dygraph.Data.create_time_ns
    in
    make_graph
      ~name:"with_overnights_hidden"
      ~title:"With overnights hidden"
      ~value_formatter
      ~axis_label_formatter
      ~data
      ()
      graph
  in
  let visible_overnights_graphs =
    let data = Dygraph.Data.create_time_ns raw_data in
    make_graph
      ~name:"with_overnights_visible"
      ~title:"With overnights visible"
      ~data
      ()
      graph
  in
  let%arr hide_overnights_graph and visible_overnights_graphs in
  Vdom.Node.div [ hide_overnights_graph; visible_overnights_graphs ]
;;
