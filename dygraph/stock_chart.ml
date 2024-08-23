open! Core
open Import

module Scale = struct
  type t =
    [ `log
    | `linear
    ]
  [@@deriving enumerate, equal, sexp]

  let to_string t = sexp_of_t t |> Sexp.to_string
end

let scale : local_ Bonsai.graph -> (Scale.t * Vdom.Node.t) Bonsai.t =
  fun (local_ graph) ->
  let scale, set_scale =
    Bonsai.state `log ~sexp_of_model:[%sexp_of: Scale.t] ~equal:[%equal: Scale.t] graph
  in
  let%arr scale and set_scale in
  let view =
    Vdom_input_widgets.Dropdown.of_enum
      ~merge_behavior:Legacy_dont_merge
      (module Scale)
      ~selected:scale
      ~on_change:set_scale
  in
  scale, view
;;

let data =
  let local_tz = Lazy.force Timezone.local in
  Dygraph.Data.create_date
    ~zone:local_tz
    (Array.of_list (List.map Stock_data.data ~f:(fun (date, a, b) -> date, [| a; b |])))
  |> Bonsai.return
;;

let options ~logscale =
  let series_options =
    Dygraph.Options.Series_options.create
      ()
      ~strokePattern:Dygraph.Options.Line_pattern.dashed
  in
  Dygraph.Options.create
    ()
    ~width:800
    ~logscale
    ~series:(Dygraph.Options.Series.create [ "Nominal", series_options ])
    ~highlightSeriesOpts:
      (Dygraph.Options.Highlight_series_options.create () ~strokeWidth:1.5)
;;

let app (local_ graph) =
  let x_label = "Month" in
  let y_labels = [ "Nominal"; "Real" ] in
  let%sub scale, scale_view = scale graph in
  let options =
    match%map scale with
    | `log -> options ~logscale:true
    | `linear -> options ~logscale:false
  in
  let%sub { graph_view; _ } =
    Dygraph.With_bonsai.create
      ~key:("graph" |> Bonsai.return)
      ~x_label:(x_label |> Bonsai.return)
      ~per_series_info:
        (y_labels |> Dygraph.Per_series_info.create_all_visible |> Bonsai.return)
      ~options
      ~data
        (* By setting the graph to global variable "g", I'm able to access the graph in the
         chrome console and look at things, e.g. g.getOptions("series").  This is purely
         for debugging/convenience. *)
      ~with_graph:(fun graph -> Js.Unsafe.set Dom_html.window "g" graph)
      ()
      graph
  in
  let%arr graph_view and scale_view in
  Vdom.Node.div [ graph_view; Vdom.Node.textf "y-axis scale: "; scale_view ]
;;
