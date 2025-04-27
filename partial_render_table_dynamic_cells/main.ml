open! Core
open! Bonsai_web
open Bonsai.Let_syntax

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

module Prt = Bonsai_web_ui_partial_render_table
module Indexed_column_id = Prt.Indexed_column_id
module Table = Prt.Basic
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
  ?(should_be_stacked = false)
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
  let render_header text = Bonsai.return (Vdom.Node.text text) in
  Column.column
    ?visible
    ?resizable
    ~header:(render_header (Fieldslib.Field.name field))
    ?sort
    ~cell:(fun ~key:_ ~data (local_ _graph) ->
      let%arr data in
      match should_be_stacked with
      | false ->
        Vdom.Node.div
          ~attrs:[ cell_attrs ]
          [ Vdom.Node.text (M.to_string (Field.get field data)) ]
      | true ->
        Vdom.Node.div
          ~attrs:
            [ cell_attrs
            ; [%css
                {|
                  display: flex;
                  flex-direction: column;
                |}]
            ]
          [ Vdom.Node.div [ Vdom.Node.text (M.to_string (Field.get field data)) ]
          ; Vdom.Node.div [ Vdom.Node.text (M.to_string (Field.get field data)) ]
          ; Vdom.Node.div [ Vdom.Node.text (M.to_string (Field.get field data)) ]
          ; Vdom.Node.div [ Vdom.Node.text (M.to_string (Field.get field data)) ]
          ; Vdom.Node.div [ Vdom.Node.text (M.to_string (Field.get field data)) ]
          ])
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
  let render_header text = Bonsai.return (Vdom.Node.text text) in
  Column.lift
    [ column_helper (module String) Row.Fields.symbol
    ; column_helper (module String) Row.Fields.symbol ~should_be_stacked:true
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
            ~cell:(fun ~key:_ ~data (local_ _graph) ->
              let%arr data in
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
  ; column_widths : (Indexed_column_id.t * [ `Px_float of float ]) list
  }

let generic_table_and_focus_attr
  ?filter
  ~row_height
  ~styling
  ~resize_column_widths_to_fit
  ~multisort_columns_when
  ~should_show_position
  ~focus
  ~get_focus_is_locked
  ~get_lock_focus
  ~get_unlock_focus
  ~attr_of_focus
  data
  (local_ graph)
  =
  let table =
    Table.component
      (module String)
      ?filter
      ~styling
      ~resize_column_widths_to_fit
      ~wrap_header:
        (let%arr multisort_columns_when in
         Table.Columns.Sortable.Wrap_header.clickable_with_icon ~multisort_columns_when ())
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
          ; column_widths
          ; key_rank = _
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
  ; column_widths = Lazy.force column_widths
  }
;;

let table
  ?filter
  ~focus_kind
  ~row_height
  ~styling
  ~resize_column_widths_to_fit
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
      ~styling
      ~resize_column_widths_to_fit
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
      ~styling
      ~resize_column_widths_to_fit
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

module Forms = Bonsai_web_ui_form.With_automatic_view

module Layout_form = struct
  module Multisort_columns_when = struct
    type t =
      [ `Shift_click
      | `Ctrl_click
      | `Shift_or_ctrl_click
      | `Disabled
      ]
    [@@deriving sexp, equal, enumerate, compare]
  end

  module Styling = struct
    type t =
      | From_theme
      | Explicit_tomato_header
      | No_styling
      | Legacy_unsafe_raw_classnames
    [@@deriving sexp, equal, enumerate, compare]
  end

  module Params = struct
    type t =
      { styling : Styling.t
      ; show_position : bool
      ; cell_based_highlighting : bool
      ; row_height : [ `Px of int ]
      ; num_rows : int
      ; multisort_columns_when : Multisort_columns_when.t
      ; resize_column_widths_to_fit : bool
      }
    [@@deriving typed_fields]

    let form_for_field
      : type a. a Typed_field.t -> local_ Bonsai.graph -> a Forms.t Bonsai.t
      =
      fun typed_field (local_ graph) ->
      match typed_field with
      | Styling -> Forms.Elements.Dropdown.enumerable (module Styling) graph
      | Show_position -> Forms.Elements.Toggle.bool ~default:true () graph
      | Cell_based_highlighting -> Forms.Elements.Toggle.bool ~default:false () graph
      | Row_height ->
        let form =
          Forms.Elements.Range.int
            ~min:(Bonsai.return 0)
            ~max:(Bonsai.return 100)
            ~step:(Bonsai.return 1)
            ~allow_updates_when_focused:`Never
            ()
            ~default:(Bonsai.return 30)
            graph
        in
        let%arr form in
        Forms.project form ~parse_exn:(fun x -> `Px x) ~unparse:(fun (`Px x) -> x)
      | Num_rows ->
        Forms.Elements.Number.int
          ~allow_updates_when_focused:`Never
          ~default:10_000
          ~step:1
          ()
          graph
      | Multisort_columns_when ->
        Forms.Elements.Dropdown.enumerable (module Multisort_columns_when) graph
      | Resize_column_widths_to_fit -> Forms.Elements.Toggle.bool ~default:false () graph
    ;;

    let label_for_field = `Inferred
  end

  let component (local_ graph) =
    let form = Forms.Typed.Record.make (module Params) graph in
    let%arr form in
    let values =
      Forms.value_or_default
        form
        ~default:
          { styling = From_theme
          ; show_position = true
          ; row_height = `Px 30
          ; num_rows = 10_000
          ; cell_based_highlighting = false
          ; multisort_columns_when = `Shift_click
          ; resize_column_widths_to_fit = false
          }
    in
    let view =
      Vdom.Node.div ~attrs:[ Style.form_container ] [ Forms.view_as_vdom form ]
    in
    view, values
  ;;
end

module Column_width_form = struct
  let component ~set_column_width (local_ graph) =
    let open Bonsai.Let_syntax in
    let form =
      Forms.Elements.Textbox.int
        ~placeholder:(Bonsai.return "Symbol column width")
        ~allow_updates_when_focused:`Always
        ()
        graph
    in
    let button =
      let theme = View.Theme.current graph in
      let%arr form and theme and set_column_width in
      let value = Forms.value form in
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
    let%arr form and button in
    View.hbox [ Forms.view_as_vdom form; button ]
  ;;
end

let component ~theme_picker (local_ graph) =
  let%sub ( form_view
          , { styling
            ; show_position
            ; row_height
            ; num_rows
            ; cell_based_highlighting
            ; multisort_columns_when
            ; resize_column_widths_to_fit
            } )
    =
    Layout_form.component graph
  in
  let data =
    let%arr num_rows in
    Row.many_random num_rows
  in
  let%sub { table
          ; focus_attr
          ; set_column_width
          ; lock_focus
          ; unlock_focus
          ; focus_is_locked
          ; column_widths
          }
    =
    let tomato_styling =
      let theme = View.Theme.current graph in
      let%arr theme in
      View.For_components.Prt.styling theme
      |> Bonsai_web_ui_partial_render_table_styling.Expert.add_attrs
           ~header_cell:[ {%css|background-color: tomato;|} ]
    in
    let no_styling =
      return
        (Bonsai_web_ui_partial_render_table_styling.Expert.lift
           { table_vars = Vdom.Attr.empty
           ; table = Vdom.Attr.empty
           ; header = Vdom.Attr.empty
           ; header_row = Vdom.Attr.empty
           ; header_cell = Vdom.Attr.empty
           ; header_cell_focused = Vdom.Attr.empty
           ; body = Vdom.Attr.empty
           ; row = Vdom.Attr.empty
           ; row_focused = Vdom.Attr.empty
           ; row_of_focused_cell = Vdom.Attr.empty
           ; cell = Vdom.Attr.empty
           ; cell_focused = Vdom.Attr.empty
           ; autosize_table_cell_wrapper = Vdom.Attr.empty
           ; autosize_table_cell_wrapper_focused = Vdom.Attr.empty
           })
    in
    let render ~cell_based_highlighting styling =
      let focus_kind = if cell_based_highlighting then `Cell else `Row in
      let styling =
        match styling with
        | Layout_form.Styling.Legacy_unsafe_raw_classnames ->
          Prt.Which_styling.Legacy_unsafe_raw_classnames
        | From_theme -> From_theme
        | Explicit_tomato_header -> This_one tomato_styling
        | No_styling -> This_one no_styling
      in
      table
        ~multisort_columns_when
        ~focus_kind
        ~styling
        ~should_show_position:show_position
        ~row_height
        ~resize_column_widths_to_fit
        data
        graph
    in
    match%sub cell_based_highlighting, styling with
    | false, Legacy_unsafe_raw_classnames ->
      render ~cell_based_highlighting:false Legacy_unsafe_raw_classnames
    | false, From_theme -> render ~cell_based_highlighting:false From_theme
    | false, Explicit_tomato_header ->
      render ~cell_based_highlighting:false Explicit_tomato_header
    | false, No_styling -> render ~cell_based_highlighting:false No_styling
    | true, Legacy_unsafe_raw_classnames ->
      render ~cell_based_highlighting:true Legacy_unsafe_raw_classnames
    | true, From_theme -> render ~cell_based_highlighting:true From_theme
    | true, Explicit_tomato_header ->
      render ~cell_based_highlighting:true Explicit_tomato_header
    | true, No_styling -> render ~cell_based_highlighting:true No_styling
  in
  let toggle_focus_lock_button =
    let on_click =
      let%arr focus_is_locked and lock_focus and unlock_focus in
      if focus_is_locked then unlock_focus else lock_focus
    in
    let theme = View.Theme.current graph in
    let%arr on_click and focus_is_locked and theme in
    let text = if focus_is_locked then "Unlock focus" else "Lock focus" in
    View.button theme ~on_click text
  in
  let form_view =
    let width_form = Column_width_form.component ~set_column_width graph in
    let%arr form_view and width_form in
    View.vbox [ form_view; width_form ]
  in
  let%arr form_view
  and table
  and focus_attr
  and theme_picker
  and toggle_focus_lock_button
  and column_widths in
  let column_widths =
    Vdom.Node.sexp_for_debugging
      [%sexp
        (column_widths
         : (Bonsai_web_ui_partial_render_table.Indexed_column_id.t
           * [ `Px_float of float ])
             list)]
  in
  Vdom.Node.div
    ~attrs:[ focus_attr ]
    [ theme_picker; form_view; toggle_focus_lock_button; column_widths; table ]
;;

let component_with_theme (local_ graph) =
  let%sub theme, theme_picker = Bonsai_web_ui_gallery.Theme_picker.component () graph in
  View.Theme.set_for_app theme (component ~theme_picker) graph
;;

let () = Bonsai_web.Start.start component_with_theme ~enable_bonsai_telemetry:Enabled
