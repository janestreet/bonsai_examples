open! Core
open! Async_kernel
open! Bonsai_web
open! Bonsai.Let_syntax

(* $MDX part-begin=row_type *)
(* Our "row key" type is [Symbol.t], which we implement as a string. *)
module Symbol = String

module Row = struct
  type t =
    { symbol : Symbol.t
    ; price : float
    ; num_owned : int
    ; last_updated : Time_ns.t
    }
  [@@deriving sexp, compare, equal, bin_io, typed_fields]
end

(* $MDX part-end *)

let row_generator =
  let open Quickcheck.Generator in
  let open Let_syntax in
  let%bind symbol_len = Int.gen_incl 2 8 in
  let%map price = Float.gen_incl (-1000.0) 10_000.0
  and num_owned = Int.gen_incl (-1_000) 1_000
  and symbol = String.gen_with_length symbol_len Char.gen_uppercase
  and last_updated =
    Time_ns.gen_uniform_incl
      (Time_ns.sub (Time_ns.now ()) (Time_ns.Span.of_day 1.))
      (Time_ns.now ())
  in
  { Row.symbol; price; num_owned; last_updated }
;;

let full_data ?seed ~num_rows () : Row.t Symbol.Map.t =
  let gen = Quickcheck.Generator.list_with_length num_rows row_generator in
  Quickcheck.random_value ?seed gen
  |> List.map ~f:(fun v -> v.symbol, v)
  |> Symbol.Map.of_alist_reduce ~f:(fun _ v -> v)
;;

module _ = struct
  (* $MDX part-begin=variant_col_id *)

  module Col_id = struct
    module T = struct
      type t =
        | Symbol
        | Price
        | Num_owned
        | Last_updated
      [@@deriving sexp, compare, enumerate]
    end

    include T
    include Comparator.Make (T)
  end

  (* $MDX part-end *)

  (* $MDX part-begin=variant_structure *)
  module Structure = Bonsai_web_ui_partial_render_table.Column_structure

  let structure =
    Structure.Group.(
      [ leaf Col_id.Symbol
      ; group
          ~label:(return {%html|Position|})
          [ leaf Col_id.Price; leaf Col_id.Num_owned ]
      ; leaf Col_id.Last_updated
      ]
      |> lift)
  ;;

  (* $MDX part-end *)

  let s' = structure

  (* $MDX part-begin=flat_structure *)
  let structure = Structure.flat Col_id.all

  (* $MDX part-end *)
  let () = ignore structure
  let structure = s'

  (* $MDX part-begin=variant_structure_mods *)
  let structure =
    structure
    |> Structure.with_initial_widths
         ~f:
           (Bonsai.return (function
             | Col_id.Symbol -> `Px 75
             | Last_updated -> `Px 150
             | _ -> Structure.default_initial_width))
    |> Structure.with_is_resizable
         ~f:
           (Bonsai.return (function
             | Col_id.Symbol | Price -> true
             | Num_owned | Last_updated -> false))
  ;;

  (* $MDX part-end *)

  (* $MDX part-begin=variant_columns *)
  module Table = Bonsai_web_ui_partial_render_table.Basic

  let columns : (Symbol.t, Row.t, Col_id.t) Table.Columns.t =
    Table.Columns.build
      (module Col_id)
      ~columns:structure
      ~render_cell:
        (Stateful_rows
           (fun _key data (local_ _graph) ->
             let%arr { Row.symbol; price; num_owned; last_updated } = data in
             fun col ->
               match col with
               | Col_id.Symbol -> Vdom.Node.text symbol
               | Price -> Vdom.Node.text (sprintf "%.2f" price)
               | Num_owned -> Vdom.Node.text (string_of_int num_owned)
               | Last_updated -> Vdom.Node.text (Time_ns.to_string last_updated)))
      ~render_header:(fun col (local_ _graph) ->
        match%arr col with
        | Symbol -> Vdom.Node.text "Symbol"
        | Price -> Vdom.Node.text "Price"
        | Num_owned -> Vdom.Node.text "Num_owned"
        | Last_updated -> Vdom.Node.text "Last Updated")
  ;;

  (* $MDX part-end *)

  (* $MDX part-begin=table_no_focus *)
  let component (local_ graph) ~data =
    let table =
      Table.component
        (module Symbol)
        ~focus:None
        ~row_height:(Bonsai.return (`Px 30))
        ~columns
        data
        graph
    in
    let%arr { view; _ } = table in
    view
  ;;

  (* $MDX part-end *)

  let () =
    Util.run (component ~data:(Bonsai.return (full_data ~num_rows:1000 ()))) ~id:"prt"
  ;;

  (* $MDX part-begin=sort_variant *)
  module Sort_kind = Bonsai_web_ui_partial_render_table.Sort_kind

  let sorts (col_id : Col_id.t Bonsai.t) (local_ _graph) =
    let%arr col_id in
    match col_id with
    | Symbol ->
      Some
        (Sort_kind.reversible ~forward:(fun (_a_key, a) (_b_key, b) ->
           [%compare: string] a.Row.symbol b.Row.symbol))
    | Price ->
      Some
        (Sort_kind.reversible ~forward:(fun (_a_key, a) (_b_key, b) ->
           [%compare: float] a.Row.price b.Row.price))
    | Num_owned -> None
    | Last_updated ->
      Some
        (Sort_kind.reversible ~forward:(fun (_a_key, a) (_b_key, b) ->
           [%compare: Time_ns.t] a.Row.last_updated b.Row.last_updated))
  ;;

  (* $MDX part-end *)

  let columns : (Symbol.t, Row.t, Col_id.t) Table.Columns.t =
    Table.Columns.build
      (module Col_id)
      ~sorts
      ~columns:structure
      ~render_cell:
        (Pure
           (return (fun col _key { Row.symbol; price; num_owned; last_updated } ->
              match col with
              | Col_id.Symbol -> Vdom.Node.text symbol
              | Price -> Vdom.Node.text (sprintf "%.2f" price)
              | Num_owned -> Vdom.Node.text (string_of_int num_owned)
              | Last_updated -> Vdom.Node.text (Time_ns.to_string last_updated))))
      ~render_header:(fun col (local_ _graph) ->
        match%arr col with
        | Symbol -> Vdom.Node.text "Symbol"
        | Price -> Vdom.Node.text "Price"
        | Num_owned -> Vdom.Node.text "Num_owned"
        | Last_updated -> Vdom.Node.text "Last Updated")
  ;;

  let component (local_ graph) ~data =
    let table =
      Table.component
        (module Symbol)
        ~focus:None
        ~row_height:(Bonsai.return (`Px 30))
        ~columns
        data
        graph
    in
    let%arr { view; _ } = table in
    view
  ;;

  let () =
    Util.run (component ~data:(Bonsai.return (full_data ~num_rows:1000 ()))) ~id:"sort"
  ;;

  (* $MDX part-begin=focus_variant *)
  let component (local_ graph) ~data =
    let table =
      Table.component
        (module Symbol)
        ~focus:
          (Table.Focus.By_cell
             { on_change =
                 Bonsai.return (fun (_ : (Symbol.t * Col_id.t) option) -> Effect.Ignore)
             })
        ~row_height:(Bonsai.return (`Px 30))
        ~columns
        data
        graph
    in
    let%arr { view; focus; num_filtered_rows; _ } = table in
    Vdom.Node.div
      ~attrs:
        [ Vdom.Attr.on_keydown (fun kbc ->
            let binding =
              let current_or_first_column =
                match Table.Focus.By_cell.focused focus with
                | None -> Col_id.Symbol
                | Some (_, c) -> c
              in
              match Js_of_ocaml.Dom_html.Keyboard_code.of_event kbc with
              | ArrowDown | KeyJ -> Some (Table.Focus.By_cell.focus_down focus)
              | ArrowUp | KeyK -> Some (Table.Focus.By_cell.focus_up focus)
              | ArrowLeft | KeyH -> Some (Table.Focus.By_cell.focus_left focus)
              | ArrowRight | KeyL -> Some (Table.Focus.By_cell.focus_right focus)
              | PageDown -> Some (Table.Focus.By_cell.page_down focus)
              | PageUp -> Some (Table.Focus.By_cell.page_up focus)
              | Escape -> Some (Table.Focus.By_cell.unfocus focus)
              | Home ->
                Some (Table.Focus.By_cell.focus_index focus 0 current_or_first_column)
              | End ->
                Some
                  (Table.Focus.By_cell.focus_index
                     focus
                     num_filtered_rows
                     current_or_first_column)
              | _ -> None
            in
            match binding with
            | Some b -> Effect.Many [ Effect.Prevent_default; b ]
            | None -> Effect.Ignore)
          (* [tabindex=0] allows browser focus to be set on the table.
             We then remove the default focus ring with [outline: none] css. *)
        ; Vdom.Attr.tabindex 0
        ; {%css|outline: none;|}
        ]
      [ view ]
  ;;

  (* $MDX part-end *)
  let () =
    Util.run
      (component ~data:(Bonsai.return (full_data ~num_rows:1000 ())))
      ~id:"focus_variant"
  ;;

  let component (local_ graph) ~data =
    (* $MDX part-begin=prt_styling *)
    let table =
      Table.component
        (module Symbol)
        ~styling:
          (This_one
             (Bonsai.return
                Bonsai_web_ui_partial_render_table_styling.(
                  create
                    { colors =
                        { page_bg = `Hex "#f0f4f8"
                        ; page_fg = `Hex "#333333"
                        ; header_bg = `Hex "#2c3e50"
                        ; header_fg = `Hex "#ecf0f1"
                        ; header_cell_focused_bg = `Hex "#2c3e50"
                        ; header_cell_focused_fg = `Hex "#2980b9"
                        ; row_even_bg = `Hex "#ffffff"
                        ; row_even_fg = `Hex "#333333"
                        ; row_odd_bg = `Hex "#e8eef2"
                        ; row_odd_fg = `Hex "#333333"
                        ; cell_focused_bg = `Hex "#3498db"
                        ; cell_focused_fg = `Hex "#ffffff"
                        ; cell_focused_outline = Some (`Hex "#2980b9")
                        ; row_focused_bg = `Hex "#d6eaf8"
                        ; row_focused_fg = `Hex "#2980b9"
                        ; row_focused_border = `Hex "#2980b9"
                        ; row_of_focused_cell_fg = None
                        ; row_of_focused_cell_bg = None
                        ; header_header_border = `Hex "#34495e"
                        ; body_body_border = `Hex "#bdc3c7"
                        ; header_body_border = `Hex "#7f8c8d"
                        }
                    ; lengths = Params.Lengths.default
                    ; fonts = Params.Fonts.default
                    })))
        ~focus:
          (Table.Focus.By_cell
             { on_change =
                 Bonsai.return (fun (_ : (Symbol.t * Col_id.t) option) -> Effect.Ignore)
             })
        ~row_height:(Bonsai.return (`Px 30))
        ~columns
        data
        graph
    in
    (* $MDX part-end *)
    let%arr { view; focus; num_filtered_rows; _ } = table in
    Vdom.Node.div
      ~attrs:
        [ Vdom.Attr.on_keydown (fun kbc ->
            let binding =
              let current_or_first_column =
                match Table.Focus.By_cell.focused focus with
                | None -> Col_id.Symbol
                | Some (_, c) -> c
              in
              match Js_of_ocaml.Dom_html.Keyboard_code.of_event kbc with
              | ArrowDown | KeyJ -> Some (Table.Focus.By_cell.focus_down focus)
              | ArrowUp | KeyK -> Some (Table.Focus.By_cell.focus_up focus)
              | ArrowLeft | KeyH -> Some (Table.Focus.By_cell.focus_left focus)
              | ArrowRight | KeyL -> Some (Table.Focus.By_cell.focus_right focus)
              | PageDown -> Some (Table.Focus.By_cell.page_down focus)
              | PageUp -> Some (Table.Focus.By_cell.page_up focus)
              | Escape -> Some (Table.Focus.By_cell.unfocus focus)
              | Home ->
                Some (Table.Focus.By_cell.focus_index focus 0 current_or_first_column)
              | End ->
                Some
                  (Table.Focus.By_cell.focus_index
                     focus
                     num_filtered_rows
                     current_or_first_column)
              | _ -> None
            in
            match binding with
            | Some b -> Effect.Many [ Effect.Prevent_default; b ]
            | None -> Effect.Ignore)
          (* Allows browser focus to be set on the table. *)
        ; Vdom.Attr.tabindex 0 (* Unsets default browser styling for focused elements. *)
        ; {%css|outline: none;|}
        ]
      [ view ]
  ;;

  let () =
    Util.run
      (component ~data:(Bonsai.return (full_data ~num_rows:1000 ())))
      ~id:"prt_styling"
  ;;
end

module _ = struct
  (* $MDX part-begin=typed_fields_col_id *)

  module Col_id = struct
    include Row.Typed_field.Packed
    include Comparator.Make (Row.Typed_field.Packed)
  end
  (* $MDX part-end *)

  (* $MDX part-begin=typed_fields_structure *)
  module Structure = Bonsai_web_ui_partial_render_table.Column_structure

  let structure =
    let open Structure.Group in
    let pack_leaf x = leaf (Col_id.pack x) in
    [ pack_leaf Symbol
    ; group ~label:(return {%html|Position|}) [ pack_leaf Price; pack_leaf Num_owned ]
    ; pack_leaf Last_updated
    ]
    |> lift
  ;;

  (* $MDX part-end *)

  (* $MDX part-begin=typed_fields_sorts *)
  module Sort_kind = Bonsai_web_ui_partial_render_table.Sort_kind

  let sort (type a) (module S : Comparable with type t = a) (field : a Row.Typed_field.t) =
    Some
      (Sort_kind.reversible ~forward:(fun (_a_key, a) (_b_key, b) ->
         S.compare (Row.Typed_field.get field a) (Row.Typed_field.get field b)))
  ;;

  let sorts (col_id : Col_id.t Bonsai.t) (local_ _graph) =
    let%arr { f = T field } = col_id in
    match field with
    | Symbol -> sort (module String) field
    | Price -> sort (module Float) field
    | Num_owned -> None
    | Last_updated -> sort (module Time_ns) field
  ;;

  (* $MDX part-end *)

  (* $MDX part-begin=typed_fields_columns *)
  module Table = Bonsai_web_ui_partial_render_table.Basic

  let columns : (Symbol.t, Row.t, Col_id.t) Table.Columns.t =
    Table.Columns.build
      (module Col_id)
      ~sorts
      ~columns:structure
      ~render_cell:
        (Pure
           (return (fun { Col_id.f = T field } _key data ->
              let value = Row.Typed_field.get field data in
              match field with
              | Symbol -> Vdom.Node.text value
              | Price -> Vdom.Node.text (sprintf "%.2f" value)
              | Num_owned -> Vdom.Node.text (string_of_int value)
              | Last_updated -> Vdom.Node.text (Time_ns.to_string value))))
      ~render_header:(fun col (local_ _graph) ->
        let%arr { f = T field } = col in
        Vdom.Node.text (Row.Typed_field.name field))
  ;;

  (* $MDX part-end *)

  let () = ignore columns
end

module _ = struct
  module Query = struct
    type t =
      { filter_params : unit
      ; sort_order : Row.Typed_field.Packed.t Bonsai_web_ui_partial_render_table.Order.t
      ; visible_range : int * int
      }
  end

  let fetch_data_polling_rpc _query = return Incr_map_collate.Collated.empty

  (* $MDX part-begin=server_side_columns *)
  module Table = Bonsai_web_ui_partial_render_table.Expert

  module Col_id = struct
    include Row.Typed_field.Packed
    include Comparator.Make (Row.Typed_field.Packed)
  end

  module Structure = Bonsai_web_ui_partial_render_table.Column_structure

  let component (local_ graph) =
    (* We need to create the sortable state outside of the table. *)
    let sortable_state =
      Table.Columns.Sortable.state ~equal:[%equal: Col_id.t] () graph
    in
    let columns : (Symbol.t, Row.t, Col_id.t) Table.Columns.t =
      Table.Columns.build
        (module Col_id)
        ~columns:(Structure.flat Col_id.all)
        ~render_cell:
          (Pure
             (return (fun { Col_id.f = T field } _key data ->
                let value = Row.Typed_field.get field data in
                match field with
                | Symbol -> Vdom.Node.text value
                | Price -> Vdom.Node.text (sprintf "%.2f" value)
                | Num_owned -> Vdom.Node.text (string_of_int value)
                | Last_updated -> Vdom.Node.text (Time_ns.to_string value))))
        ~render_header:(fun col (local_ _graph) ->
          let%arr ({ f = T field } as col) = col
          and sortable_state in
          Table.Columns.Sortable.Header.Expert.default_click_handler
            ~sortable:true
            ~column_id:col
            sortable_state
            (Table.Columns.Sortable.Header.with_icon
               (Vdom.Node.text (Row.Typed_field.name field))))
    in
    (* $MDX part-end *)
    ignore columns;
    let focus = Table.Focus.None in
    let filter_params = return () in
    (* $MDX part-begin=server_side_query *)
    let copied_range, set_copied_range = Bonsai.state (0, 0) graph in
    let query =
      let%arr copied_range and sortable_state and filter_params in
      { Query.filter_params
      ; sort_order = Table.Columns.Sortable.order sortable_state
      ; visible_range = copied_range
      }
    in
    (* In practice, this would probably need some kind of error handling.*)
    let data = fetch_data_polling_rpc query in
    let table =
      Table.component
        (module Symbol)
        ~focus
        ~row_height:(Bonsai.return (`Px 30))
        ~columns
        data
        graph
    in
    let%sub { range = table_range; _ } = table in
    Bonsai.Edge.on_change
      ~equal:[%equal: int * int]
      table_range
      ~callback:
        (let%arr set_copied_range in
         fun table_range -> set_copied_range table_range)
      graph (* $MDX part-end *);
    ignore query
  ;;

  let () = ignore component
end
