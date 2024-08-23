open! Core
open! Async_kernel
open! Bonsai_web
open! Bonsai.Let_syntax

(* $MDX part-begin=row_type *)

module Symbol = String

module Row = struct
  type t =
    { symbol : Symbol.t
    ; price : float
    ; num_owned : int
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
  and symbol = String.gen_with_length symbol_len Char.gen_uppercase in
  { Row.symbol; price; num_owned }
;;

let full_data ?seed ~num_rows () : Row.t Symbol.Map.t =
  let gen = Quickcheck.Generator.list_with_length num_rows row_generator in
  Quickcheck.random_value ?seed gen
  |> List.map ~f:(fun v -> v.symbol, v)
  |> Symbol.Map.of_alist_reduce ~f:(fun _ v -> v)
;;

module _ = struct
  (* $MDX part-begin=dynamic_experimental_variant_col_id *)
  module Table = Bonsai_web_ui_partial_render_table.Basic

  module Col_id = struct
    module T = struct
      type t =
        | Symbol
        | Price
        | Num_owned
      [@@deriving sexp, compare]
    end

    include T
    include Comparator.Make (T)
  end

  let columns : (Symbol.t, Row.t, Col_id.t) Table.Columns.t =
    Table.Columns.Dynamic_experimental.build
      (module Col_id)
      ~columns:(Bonsai.return [ Col_id.Symbol; Price; Num_owned ])
      ~render_cell:(fun col _key data (local_ _graph) ->
        match%sub col with
        | Symbol ->
          let%arr { Row.symbol; _ } = data in
          Vdom.Node.text symbol
        | Price ->
          let%arr { price; _ } = data in
          Vdom.Node.text (sprintf "%.2f" price)
        | Num_owned ->
          let%arr { num_owned; _ } = data in
          Vdom.Node.text (string_of_int num_owned))
      ~render_header:(fun col (local_ _graph) ->
        let%arr col in
        let name =
          match col with
          | Symbol -> Vdom.Node.text "Symbol"
          | Price -> Vdom.Node.text "Price"
          | Num_owned -> Vdom.Node.text "Num_owned"
        in
        Table.Columns.Dynamic_columns.Sortable.Header.with_icon name)
  ;;

  (* $MDX part-end *)

  (* $MDX part-begin=dynamic_experimental_table_no_focus *)
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
    Util.run
      (component ~data:(Bonsai.return (full_data ~num_rows:1000 ())))
      ~id:"dynamic_experimental"
  ;;

  (* $MDX part-begin=dynamic_experimental_sort_variant *)
  module Sort_kind = Table.Columns.Dynamic_experimental.Sort_kind

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
  ;;

  (* $MDX part-end *)

  let columns : (Symbol.t, Row.t, Col_id.t) Table.Columns.t =
    Table.Columns.Dynamic_experimental.build
      (module Col_id)
      ~sorts
      ~columns:(Bonsai.return [ Col_id.Symbol; Price; Num_owned ])
      ~render_cell:(fun col _key data (local_ _graph) ->
        match%sub col with
        | Symbol ->
          let%arr { Row.symbol; _ } = data in
          Vdom.Node.text symbol
        | Price ->
          let%arr { price; _ } = data in
          Vdom.Node.text (sprintf "%.2f" price)
        | Num_owned ->
          let%arr { num_owned; _ } = data in
          Vdom.Node.text (string_of_int num_owned))
      ~render_header:(fun col (local_ _graph) ->
        let%arr col in
        let name =
          match col with
          | Symbol -> Vdom.Node.text "Symbol"
          | Price -> Vdom.Node.text "Price"
          | Num_owned -> Vdom.Node.text "Num_owned"
        in
        Table.Columns.Dynamic_columns.Sortable.Header.with_icon name)
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
    Util.run
      (component ~data:(Bonsai.return (full_data ~num_rows:1000 ())))
      ~id:"dynamic_experimental_sort"
  ;;

  (* $MDX part-begin=dynamic_experimental_focus_variant *)
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
          (* Allows browser focus to be set on the table. *)
        ; Vdom.Attr.tabindex 0 (* Unsets default browser styling for focused elements. *)
        ; {%css|outline: none;|}
        ]
      [ view ]
  ;;

  (* $MDX part-end *)
  let () =
    Util.run
      (component ~data:(Bonsai.return (full_data ~num_rows:1000 ())))
      ~id:"dynamic_experimental_focus_variant"
  ;;
end

module _ = struct
  (* $MDX part-begin=dynamic_experimental_typed_fields_col_id *)
  module Table = Bonsai_web_ui_partial_render_table.Basic

  module Col_id = struct
    include Row.Typed_field.Packed
    include Comparator.Make (Row.Typed_field.Packed)
  end
  (* $MDX part-end *)

  (* $MDX part-begin=dynamic_experimental_typed_fields_sorts *)
  module Sort_kind = Table.Columns.Dynamic_experimental.Sort_kind

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
  ;;

  (* $MDX part-end *)

  (* $MDX part-begin=dynamic_experimental_typed_fields_columns *)
  let all_columns = Bonsai.return Row.Typed_field.Packed.all

  let columns : (Symbol.t, Row.t, Col_id.t) Table.Columns.t =
    Table.Columns.Dynamic_experimental.build
      (module Col_id)
      ~sorts
      ~columns:all_columns
      ~render_cell:(fun col _key data (local_ _graph) ->
        let%arr { f = T field } = col
        and data in
        let value = Row.Typed_field.get field data in
        match field with
        | Symbol -> Vdom.Node.text value
        | Price -> Vdom.Node.textf "%f" value
        | Num_owned -> Vdom.Node.textf "%d" value)
      ~render_header:(fun col (local_ _graph) ->
        let%arr { f = T field } = col in
        Table.Columns.Dynamic_columns.Sortable.Header.with_icon
          (Vdom.Node.text (Row.Typed_field.name field)))
  ;;

  (* $MDX part-end *)

  let () = ignore columns
end

module _ = struct
  (* $MDX part-begin=server_side_columns *)
  module Table = Bonsai_web_ui_partial_render_table.Expert
  module Column = Table.Columns.Dynamic_experimental

  module Col_id = struct
    include Row.Typed_field.Packed
    include Comparator.Make (Row.Typed_field.Packed)
  end

  let all_columns = Bonsai.return Row.Typed_field.Packed.all

  let component (local_ graph) ~data =
    let sortable_state = Column.Sortable.state ~equal:[%equal: Col_id.t] () graph in
    let columns : (Symbol.t, Row.t, Col_id.t) Table.Columns.t =
      Column.build
        (module Col_id)
        ~columns:all_columns
        ~render_cell:(fun col _key data (local_ _graph) ->
          let%arr { f = T field } = col
          and data in
          let value = Row.Typed_field.get field data in
          match field with
          | Symbol -> Vdom.Node.text value
          | Price -> Vdom.Node.textf "%f" value
          | Num_owned -> Vdom.Node.textf "%d" value)
        ~render_header:(fun col (local_ _graph) ->
          let%arr ({ f = T field } as col) = col
          and sortable_state in
          Column.Sortable.Header.Expert.default_click_handler
            ~sortable:true
            ~column_id:col
            sortable_state
            (Column.Sortable.Header.with_icon
               (Vdom.Node.text (Row.Typed_field.name field))))
    in
    (* $MDX part-end *)
    ignore data;
    ignore columns
  ;;

  let () = ignore component
end
