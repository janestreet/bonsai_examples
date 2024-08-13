open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Table = Bonsai_web_ui_partial_render_table.Basic
module Indexed_column_id = Bonsai_web_ui_partial_render_table.Indexed_column_id
module Form = Bonsai_web_ui_form.With_automatic_view
module Row = Row

module Time_ns_option = struct
  type t = Time_ns.t option [@@deriving compare]

  let to_string = function
    | None -> "-"
    | Some t -> Time_ns.to_string_utc t
  ;;
end

module Style =
  [%css
  stylesheet
    {|
      .form_container {
        position: fixed;
        top: 0px;
        right: 0px;
        z-index: 9000;
      }
      |}]

module type S = sig
  type t [@@deriving compare]

  val to_string : t -> string
end

module Column = Table.Columns.Dynamic_cells

let cell_attrs =
  {%css|
      display: flex;
      align-items: center;
      white-space: pre;
    |}
;;

let column_helper
  (type a)
  (module M : S with type t = a)
  ?(disable_sort = false)
  ?visible
  ?resizable
  (field : (_, a) Field.t)
  =
  let sort =
    if disable_sort
    then None
    else
      Some
        (Bonsai.return (fun (_, a) (_, b) ->
           M.compare (Field.get field a) (Field.get field b)))
  in
  let render_header text =
    Bonsai.return (Column.Sortable.Header.with_icon (Vdom.Node.text text))
  in
  Column.column
    ?visible
    ?resizable
    ~header:(render_header (Fieldslib.Field.name field))
    ?sort
    ~cell:(fun ~key:_ ~data _graph ->
      let%arr data = data in
      Vdom.Node.div
        ~attrs:[ cell_attrs ]
        [ Vdom.Node.text (M.to_string (Field.get field data)) ])
    ()
;;

let special_compare_option how compare_inner a b =
  match a, b with
  | None, None -> 0
  | Some _, None -> -1
  | None, Some _ -> 1
  | Some a, Some b ->
    (match how with
     | `Ascending -> compare_inner a b
     | `Descending -> -compare_inner a b)
;;

let columns ~should_show_position =
  let render_header text =
    Bonsai.return (Column.Sortable.Header.with_icon (Vdom.Node.text text))
  in
  Column.lift
    [ column_helper (module String) Row.Fields.symbol
    ; column_helper (module Float) Row.Fields.edge
    ; column_helper (module Float) Row.Fields.max_edge
    ; column_helper (module Int) Row.Fields.bsize ~resizable:(Bonsai.return false)
    ; column_helper (module Float) Row.Fields.bid ~resizable:(Bonsai.return false)
    ; column_helper (module Float) Row.Fields.ask ~resizable:(Bonsai.return false)
    ; column_helper (module Int) Row.Fields.asize ~resizable:(Bonsai.return false)
    ; Column.group
        ~label:(Bonsai.return (Vdom.Node.text "some group"))
        [ Column.group
            ~label:(Bonsai.return (Vdom.Node.text "small"))
            [ column_helper
                (module Int)
                Row.Fields.position
                ~disable_sort:true
                ~visible:should_show_position
            ]
        ; Column.column
            ~header:(render_header "last fill")
            ~sort:
              (Bonsai.return (fun (_key1, a) (_key2, b) ->
                 special_compare_option
                   `Ascending
                   [%compare: Time_ns.t]
                   a.Row.last_fill
                   b.Row.last_fill))
            ~sort_reversed:
              (Bonsai.return (fun (_key1, a) (_key2, b) ->
                 special_compare_option
                   `Descending
                   [%compare: Time_ns.t]
                   a.Row.last_fill
                   b.Row.last_fill))
            ~cell:(fun ~key:_ ~data _graph ->
              let%arr data = data in
              Vdom.Node.div
                ~attrs:[ cell_attrs ]
                [ Vdom.Node.text (Time_ns_option.to_string data.Row.last_fill) ])
            ()
        ]
    ; column_helper (module String) Row.Fields.trader
    ]
;;

type t =
  { table : Vdom.Node.t
  ; focus_attr : Vdom.Attr.t
  ; set_column_width :
      column_id:Indexed_column_id.t -> [ `Px_float of float ] -> unit Ui_effect.t
  ; lock_focus : unit Ui_effect.t
  ; unlock_focus : unit Ui_effect.t
  ; focus_is_locked : bool
  }

let generic_table_and_focus_attr
  ?filter
  ~row_height
  ~theming
  ~autosize
  ~multisort_columns_when
  ~should_show_position
  ~focus
  ~get_focus_is_locked
  ~get_lock_focus
  ~get_unlock_focus
  ~attr_of_focus
  data
  graph
  =
  let table =
    Table.component
      (module String)
      ?filter
      ~theming
      ~autosize
      ~multisort_columns_when
      ~focus
      ~row_height
      ~columns:(columns ~should_show_position)
      data
      graph
  in
  let%arr { Table.Result.view = table
          ; for_testing = _
          ; sortable_state = _
          ; num_filtered_rows
          ; focus
          ; set_column_width
          }
    =
    table
  in
  let focus_attr = attr_of_focus focus ~num_filtered_rows in
  { table
  ; focus_attr
  ; set_column_width
  ; lock_focus = get_lock_focus focus
  ; unlock_focus = get_unlock_focus focus
  ; focus_is_locked = get_focus_is_locked focus
  }
;;

let component
  ?filter
  ~focus_kind
  ~row_height
  ~theming
  ~autosize
  ~multisort_columns_when
  ~should_show_position
  data
  =
  match focus_kind with
  | `Row ->
    let module Focus_control = Table.Focus.By_row in
    generic_table_and_focus_attr
      ?filter
      ~row_height
      ~theming
      ~autosize
      ~multisort_columns_when
      ~should_show_position
      ~focus:(By_row { on_change = Bonsai.return (Fn.const Effect.Ignore) })
      ~get_lock_focus:Focus_control.lock_focus
      ~get_unlock_focus:Focus_control.unlock_focus
      ~get_focus_is_locked:Focus_control.focus_is_locked
      ~attr_of_focus:(fun (focus : _ Table.Focus.By_row.t) ~num_filtered_rows ->
        Vdom.Attr.on_keydown (fun kbc ->
          let binding =
            match Js_of_ocaml.Dom_html.Keyboard_code.of_event kbc with
            | ArrowDown | KeyJ -> Some (Focus_control.focus_down focus)
            | ArrowUp | KeyK -> Some (Focus_control.focus_up focus)
            | PageDown -> Some (Focus_control.page_down focus)
            | PageUp -> Some (Focus_control.page_up focus)
            | Escape -> Some (Focus_control.unfocus focus)
            | Home -> Some ((Focus_control.focus_index focus) 0 ())
            | End -> Some ((Focus_control.focus_index focus) num_filtered_rows ())
            | _ -> None
          in
          match binding with
          | Some b -> Effect.Many [ Effect.Prevent_default; b ]
          | None -> Effect.Ignore))
      data
  | `Cell ->
    let module Focus_control = Table.Focus.By_cell in
    generic_table_and_focus_attr
      ?filter
      ~row_height
      ~theming
      ~autosize
      ~multisort_columns_when
      ~should_show_position
      ~focus:(By_cell { on_change = Bonsai.return (Fn.const Effect.Ignore) })
      ~get_lock_focus:Focus_control.lock_focus
      ~get_unlock_focus:Focus_control.unlock_focus
      ~get_focus_is_locked:Focus_control.focus_is_locked
      ~attr_of_focus:(fun focus ~num_filtered_rows ->
        let current_or_first_column =
          match Focus_control.focused focus with
          | None -> Indexed_column_id.of_int 0
          | Some (_, c) -> c
        in
        Vdom.Attr.on_keydown (fun kbc ->
          let binding =
            let module Focus_control = Table.Focus.By_cell in
            match Js_of_ocaml.Dom_html.Keyboard_code.of_event kbc with
            | ArrowDown | KeyJ -> Some (Focus_control.focus_down focus)
            | ArrowUp | KeyK -> Some (Focus_control.focus_up focus)
            | ArrowRight | KeyL -> Some (Focus_control.focus_right focus)
            | ArrowLeft | KeyH -> Some (Focus_control.focus_left focus)
            | PageDown -> Some (Focus_control.page_down focus)
            | PageUp -> Some (Focus_control.page_up focus)
            | Escape -> Some (Focus_control.unfocus focus)
            | Home -> Some ((Focus_control.focus_index focus) 0 current_or_first_column)
            | End ->
              Some
                ((Focus_control.focus_index focus)
                   num_filtered_rows
                   current_or_first_column)
            | _ -> None
          in
          match binding with
          | Some b -> Effect.Many [ Effect.Prevent_default; b ]
          | None -> Effect.Ignore))
      data
;;

module Layout_form = struct
  module Multisort_columns_when = struct
    type t =
      [ `Shift_click
      | `Ctrl_click
      | `Shift_or_ctrl_click
      ]
    [@@deriving sexp, equal, enumerate, compare]
  end

  module Params = struct
    type t =
      { themed : bool
      ; show_position : bool
      ; cell_based_highlighting : bool
      ; row_height : [ `Px of int ]
      ; num_rows : int
      ; multisort_columns_when : Multisort_columns_when.t
      ; autosize : bool
      }
    [@@deriving typed_fields]

    let form_for_field : type a. a Typed_field.t -> Bonsai.graph -> a Form.t Bonsai.t =
      fun typed_field graph ->
      match typed_field with
      | Themed -> Form.Elements.Toggle.bool ~default:true () graph
      | Show_position -> Form.Elements.Toggle.bool ~default:true () graph
      | Cell_based_highlighting -> Form.Elements.Toggle.bool ~default:false () graph
      | Row_height ->
        let form =
          Form.Elements.Range.int
            ~min:0
            ~max:100
            ~step:1
            ~allow_updates_when_focused:`Never
            ()
            ~default:30
            graph
        in
        let%arr form = form in
        Form.project form ~parse_exn:(fun x -> `Px x) ~unparse:(fun (`Px x) -> x)
      | Num_rows ->
        Form.Elements.Number.int
          ~allow_updates_when_focused:`Never
          ~default:10_000
          ~step:1
          ()
          graph
      | Multisort_columns_when ->
        Form.Elements.Dropdown.enumerable (module Multisort_columns_when) graph
      | Autosize -> Form.Elements.Toggle.bool ~default:false () graph
    ;;

    let label_for_field = `Inferred
  end

  let component graph =
    let form = Form.Typed.Record.make (module Params) graph in
    let%arr form = form in
    let values =
      Form.value_or_default
        form
        ~default:
          { themed = true
          ; show_position = true
          ; row_height = `Px 30
          ; num_rows = 10_000
          ; cell_based_highlighting = false
          ; multisort_columns_when = `Shift_click
          ; autosize = false
          }
    in
    let view = Vdom.Node.div ~attrs:[ Style.form_container ] [ Form.view_as_vdom form ] in
    view, values
  ;;
end

module Column_width_form = struct
  let component ~set_column_width graph =
    let open Bonsai.Let_syntax in
    let form =
      Form.Elements.Textbox.int
        ~placeholder:(Bonsai.return "Symbol column width")
        ~allow_updates_when_focused:`Always
        ()
        graph
    in
    let button =
      let theme = View.Theme.current graph in
      let%arr form = form
      and theme = theme
      and set_column_width = set_column_width in
      let value = Form.value form in
      let disabled = Or_error.is_error value in
      let on_click =
        match value with
        | Error _ -> Effect.Ignore
        | Ok value ->
          set_column_width
            ~column_id:(Indexed_column_id.of_int 0)
            (`Px_float (Int.to_float value))
      in
      View.button ~disabled theme ~on_click "Set width"
    in
    let%arr form = form
    and button = button in
    View.hbox [ Form.view_as_vdom form; button ]
  ;;
end
