open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Table = Bonsai_web_ui_partial_render_table
module Row = Row

module Time_ns_option = struct
  type t = Time_ns.t option [@@deriving compare]

  let to_string = function
    | None -> "-"
    | Some t -> Time_ns.to_string_utc t
  ;;
end

module Row_fields = struct
  include Row.Typed_field.Packed
  include Comparator.Make (Row.Typed_field.Packed)
end

module Col_id = struct
  module T = struct
    type t =
      | Row_fields of Row_fields.t
      | History
    [@@deriving sexp, compare, enumerate]
  end

  include T
  include Comparable.Make (T)
end

let dynamic_cols (local_ graph) =
  let default_column_structure =
    let pack x = Col_id.Row_fields (Row_fields.pack x) in
    Table.Column_structure.Group_dynamic.
      [ leaf (pack Symbol)
      ; group ~label:(Vdom.Node.text "Edge") [ leaf (pack Edge); leaf (pack Max_edge) ]
      ; group
          ~label:(Vdom.Node.text "Order Book")
          [ leaf (pack Bsize); leaf (pack Bid); leaf (pack Ask); leaf (pack Asize) ]
      ; leaf (pack Position)
      ; leaf (pack Last_fill)
      ; leaf (pack Trader)
      ; leaf Col_id.History
      ]
  in
  let column_structure, shuffle_column_structure =
    Bonsai.state_machine
      ~default_model:default_column_structure
      ~apply_action:(fun _ctx model () -> List.permute model)
      graph
  in
  let structure = Table.Column_structure.Group_dynamic.lift column_structure in
  let shuffle_button =
    let%arr shuffle_column_structure in
    Vdom.Node.div
      [ Vdom.Node.button
          ~attrs:[ Vdom.Attr.on_click (fun _ -> shuffle_column_structure ()) ]
          [ Vdom.Node.text "Shuffle Columns" ]
      ]
  in
  structure, shuffle_button
;;

let pure_rows (local_ graph) =
  (* Note that even though we _can_ instantiate a separate popover for each row (or even
     per cell), we only want to show one popover at a time, so we can define it outside.
     The downside is that whenever the focused row changes, the entire table will be
     recomputed, but this is usually ok, because these popovers won't be opened / closed
     in a tight loop.

     If we wanted to support multiple history popovers open at a time, we'd need to create
     separate ones per row via [Stateful_rows]. *)
  let history_open_symbol, set_history_open_symbol = Bonsai.state_opt graph in
  let anchor_history_popover =
    match%sub history_open_symbol with
    | Some symbol ->
      Bonsai_web_ui_toplayer.Popover.always_open
        ~autoclose:
          (Bonsai_web_ui_toplayer.Autoclose.create
             ~close:
               (let%arr set_history_open_symbol in
                set_history_open_symbol None)
             graph)
        ~content:(fun (local_ _graph) ->
          let%arr symbol in
          {%html|
            <div>
              <h3>#{symbol} History</h3>
              <p>The history is too sensitive to show in this demo.</p>
            </div>
          |})
        ~overflow_auto_wrapper:(Bonsai.return false)
        graph
    | None -> return Vdom.Attr.empty
  in
  Table.Render_cell.Pure
    (let%arr history_open_symbol and set_history_open_symbol and anchor_history_popover in
     fun col_id symbol data ->
       match col_id with
       | Col_id.History ->
         let popover_attr =
           if Option.mem history_open_symbol symbol ~equal:String.equal
           then anchor_history_popover
           else Vdom.Attr.empty
         in
         {%html|
           <div style="display: flex; align-items: center">
             <button
               on_click=%{fun _ -> set_history_open_symbol (Some symbol)}
               %{popover_attr}
               style="
                 margin: 2px;
                 paddding: 2px 3px;
               "
             >
               See History
             </button>
           </div>
         |}
       | Row_fields col ->
         let { f = T field } = col in
         let string, float, int =
           Vdom.Node.text, Vdom.Node.textf "%f", Vdom.Node.textf "%d"
         in
         let value = Row.Typed_field.get field data in
         (match field with
          | Symbol -> string value
          | Edge -> float value
          | Max_edge -> float value
          | Bsize -> int value
          | Bid -> float value
          | Ask -> float value
          | Asize -> int value
          | Position -> int value
          | Last_fill -> Vdom.Node.text (Time_ns_option.to_string value)
          | Trader -> string value))
;;

let component ?filter (data : Row.t String.Map.t Bonsai.t) (local_ graph) =
  let render_cell = pure_rows graph in
  let column_structure, shuffle_button = dynamic_cols graph in
  let columns =
    Table.Basic.Columns.build
      (module Col_id)
      ~columns:
        (column_structure
         |> Table.Column_structure.with_initial_widths
              ~f:
                (Bonsai.return (function
                  | Col_id.Row_fields { f = T Symbol } -> `Px 25
                  | Row_fields { f = T Trader } -> `Px 100
                  | History -> `Px 125
                  | _ -> `Px 50)))
      ~render_cell
      ~render_header:(fun col (local_ _graph) ->
        match%sub col with
        | History -> return {%html|History|}
        | Row_fields col ->
          let%arr { f = T field } = col in
          Vdom.Node.text (Row.Typed_field.name field))
  in
  let table =
    Table.Basic.component
      (module String)
      ?filter
      ~resize_column_widths_to_fit:(return true)
      ~styling:(This_one (return Bonsai_web_ui_partial_render_table_styling.default))
      ~focus:(By_row { on_change = Bonsai.return (Fn.const Effect.Ignore) })
      ~row_height:(Bonsai.return (`Px 30))
      ~columns
      data
      graph
  in
  let controls =
    let%arr { focus; num_filtered_rows; _ } = table in
    Vdom.Attr.on_keydown (fun kbc ->
      let binding =
        let module Focus_control = Bonsai_web_ui_partial_render_table.Focus_by_row in
        match Js_of_ocaml.Dom_html.Keyboard_code.of_event kbc with
        | ArrowDown | KeyJ -> Some (Focus_control.focus_down focus)
        | ArrowUp | KeyK -> Some (Focus_control.focus_up focus)
        | PageDown -> Some (Focus_control.page_down focus)
        | PageUp -> Some (Focus_control.page_up focus)
        | Escape -> Some (Focus_control.unfocus focus)
        | Home -> Some (Focus_control.focus_index focus 0 ())
        | End -> Some (Focus_control.focus_index focus num_filtered_rows ())
        | _ -> None
      in
      match binding with
      | Some b -> Effect.Many [ Effect.Prevent_default; b ]
      | None -> Effect.Ignore)
  in
  let table_view =
    let%arr { view; _ } = table in
    view
  in
  let%arr table_view and controls and shuffle_button in
  {%html|<div %{controls}>%{shuffle_button}%{table_view}</div>|}
;;

let () =
  let input = Bonsai.return (Row.many_random 100_000) in
  component input
  |> View.Theme.set_for_app (Bonsai.return (Kado.theme ~style:Light ~version:V1 ()))
  |> Bonsai_web.Start.start ~enable_bonsai_telemetry:Enabled
;;
