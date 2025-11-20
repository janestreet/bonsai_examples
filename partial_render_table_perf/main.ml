open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_manual_view
open Bonsai_web_ui_partial_render_table_configs_for_testing
module Config = All_apis_configs
module Snips = Bonsai_experimental_snips

module Which_api = struct
  module T = struct
    type t =
      | New_api
      | Old_dynamic_cells
      | Old_dynamic_cols
      | Old_dynamic_experimental
    [@@deriving equal, compare, sexp_of, enumerate, hash, string]
  end

  include T

  let form (local_ graph) =
    let which_api = Form.Elements.Dropdown.enumerable (module T) graph in
    let render_cell_kind =
      Form.Elements.Dropdown.enumerable
        ~init:(`This (return Config.Render_cell_kind.Stateful_cells))
        (module Config.Render_cell_kind)
        graph
    in
    let new_api_cols =
      Form.Elements.Dropdown.enumerable
        ~init:(`This (return Config.New_api_cols.Dynamic))
        (module Config.New_api_cols)
        graph
    in
    let constant_foldable_cols = Form.Elements.Checkbox.bool ~default:false () graph in
    let counters_in_cells = Form.Elements.Checkbox.bool ~default:false () graph in
    let which_dynamic_cols =
      Form.Elements.Dropdown.enumerable
        ~init:(`This (return Config.Which_dynamic_cols.No_counters))
        (module Config.Which_dynamic_cols)
        graph
    in
    let col_groups = Form.Elements.Checkbox.bool ~default:false () graph in
    let duplicate_col = Form.Elements.Checkbox.bool ~default:false () graph in
    let selected =
      let%arr which_api
      and render_cell_kind
      and new_api_cols
      and counters_in_cells
      and which_dynamic_cols
      and constant_foldable_cols
      and col_groups
      and duplicate_col in
      let render_cell_kind =
        Form.value_or_default render_cell_kind ~default:Stateful_cells
      in
      let new_api_cols = Form.value_or_default new_api_cols ~default:Dynamic in
      let counters_in_cells = Form.value_or_default counters_in_cells ~default:false in
      let which_dynamic_cols =
        Form.value_or_default which_dynamic_cols ~default:No_counters
      in
      let constant_foldable_cols =
        Form.value_or_default constant_foldable_cols ~default:false
      in
      let col_groups = Form.value_or_default col_groups ~default:false in
      let duplicate_col = Form.value_or_default duplicate_col ~default:false in
      match Form.value_or_default which_api ~default:New_api with
      | New_api ->
        Config.New_api
          { counters_in_cells
          ; cols = new_api_cols
          ; col_groups
          ; render_cell_kind
          ; duplicate_col
          }
      | Old_dynamic_cells ->
        Config.Dynamic_cells { counters_in_cells; col_groups; duplicate_col }
      | Old_dynamic_cols -> Dynamic_cols { col_groups; which_dynamic_cols; duplicate_col }
      | Old_dynamic_experimental ->
        Dynamic_experimental { counters_in_cells; constant_foldable_cols }
    in
    let view =
      let%arr which_api
      and render_cell_kind
      and new_api_cols
      and counters_in_cells
      and which_dynamic_cols
      and constant_foldable_cols
      and col_groups
      and duplicate_col in
      let view ~label form =
        Form.map_view form ~f:(fun view ->
          View.hbox ~gap:(`Px 4) [ Vdom.Node.text label; view ])
        |> Form.view
      in
      let render_cell_kind = Form.view render_cell_kind in
      let new_api_cols = view ~label:"Cols?" new_api_cols in
      let counters_in_cells = view ~label:"Counters?" counters_in_cells in
      let constant_foldable_cols =
        view ~label:"Cols Constant Folded?" constant_foldable_cols
      in
      let which_dynamic_cols = Form.view which_dynamic_cols in
      let col_groups = view ~label:"Grouped columns?" col_groups in
      let duplicate_col = view ~label:"Duplicate 'Edge'?" duplicate_col in
      let to_show =
        match Form.value_or_default which_api ~default:New_api with
        | New_api ->
          [ render_cell_kind; new_api_cols; counters_in_cells; col_groups; duplicate_col ]
        | Old_dynamic_cells -> [ counters_in_cells; col_groups; duplicate_col ]
        | Old_dynamic_cols -> [ which_dynamic_cols; col_groups; duplicate_col ]
        | Old_dynamic_experimental -> [ counters_in_cells; constant_foldable_cols ]
      in
      View.hbox
        ~gap:(`Px 12)
        (Form.view which_api :: to_show |> List.intersperse ~sep:(Vdom.Node.text "|"))
    in
    selected, view
  ;;
end

module Params = struct
  module T = struct
    type t =
      { num_rows : int
      ; row_height : [ `Px of int ]
      ; autosize : bool
      }
    [@@deriving typed_fields]

    type field_view = Vdom.Node.t
    type resulting_view = Vdom.Node.t

    let form_for_field
      : type a. a Typed_field.t -> local_ Bonsai.graph -> (a, field_view) Form.t Bonsai.t
      =
      fun typed_field (local_ graph) ->
      match typed_field with
      | Num_rows -> Form.Elements.Number.int ~min:0 ~step:1 ~default:10_000 () graph
      | Row_height ->
        let form =
          Form.Elements.Range.int
            ~min:(return 0)
            ~max:(return 100)
            ~step:(return 1)
            ()
            ~default:(return 30)
            graph
        in
        let%arr form in
        Form.project form ~parse_exn:(fun x -> `Px x) ~unparse:(fun (`Px x) -> x)
      | Autosize -> Form.Elements.Toggle.bool ~default:false () graph
    ;;

    type form_of_field_fn =
      { f : 'a. 'a Typed_field.t @ local -> ('a, field_view) Form.t Bonsai.t }

    let finalize_view { f } (local_ _graph) =
      let%arr form_fields =
        List.map Typed_field.Packed.all ~f:(fun { f = T field } ->
          let%arr { Form.view; _ } = f field in
          View.hbox
            ~gap:(`Px 4)
            [ Vdom.Node.text
                (Typed_field.name field
                 |> Capitalization.apply_to_snake_case Lower_sentence_case)
            ; view
            ])
        |> Bonsai.all
      in
      View.vbox form_fields
    ;;
  end

  let form (local_ graph) =
    let form = Form.Typed.Record.make (module T) graph in
    let view = Bonsai.map form ~f:Form.view in
    let value =
      Bonsai.map
        form
        ~f:
          (Form.value_or_default
             ~default:{ T.num_rows = 10_000; row_height = `Px 30; autosize = false })
    in
    value, view
  ;;
end

let component (local_ graph) =
  let which_api, which_api_view = Which_api.form graph in
  let params, params_view = Params.form graph in
  let data =
    let%arr { num_rows; _ } = params in
    Symbol_table.Row.many_random num_rows
  in
  let all_configs = Config.all in
  let match_ =
    let%arr which_api in
    List.findi_exn all_configs ~f:(fun _ -> Config.equal which_api) |> fst
  in
  let local_ with_ i =
    let config = List.nth_exn all_configs i in
    let rank_range, set_rank_range =
      Bonsai.state (Incr_map_collate.Collate_params.Which_range.To 100) graph
    in
    let%sub { view; inject; range } =
      Config.computation
        config
        (let%arr data and rank_range and params in
         Sharable.Input.create
           ~rank_range
           ~row_height:params.row_height
           ~resize_column_widths_to_fit:params.autosize
           data)
        graph
    in
    Bonsai.Edge.on_change
      ~sexp_of_model:[%sexp_of: int * int]
      ~equal:[%equal: int * int]
      range
      ~callback:
        (let%map set_rank_range in
         fun (low, high) ->
           set_rank_range
             (Incr_map_collate.Collate_params.Which_range.Between (low, high)))
      graph;
    let%arr view and inject and which_api_view and params_view in
    let controls =
      Vdom.Attr.on_keydown (fun kbc ->
        let binding =
          match Js_of_ocaml.Dom_html.Keyboard_code.of_event kbc with
          | ArrowDown | KeyJ -> Some (inject Sharable.Navigation_action.Focus_down)
          | ArrowUp | KeyK -> Some (inject Sharable.Navigation_action.Focus_up)
          | ArrowRight | KeyL -> Some (inject Sharable.Navigation_action.Focus_right)
          | ArrowLeft | KeyH -> Some (inject Sharable.Navigation_action.Focus_left)
          | PageDown -> Some (inject Sharable.Navigation_action.Page_down)
          | PageUp -> Some (inject Sharable.Navigation_action.Page_up)
          | Escape -> Some (inject Sharable.Navigation_action.Unfocus)
          | Home -> Some (inject (Sharable.Navigation_action.Focus_index_first_column 0))
          | End ->
            Some
              (inject
                 (Sharable.Navigation_action.Focus_index_first_column
                    Int.max_value_30_bits))
          | _ -> None
        in
        match binding with
        | Some b -> Effect.Many [ Effect.Prevent_default; b ]
        | None -> Effect.Ignore)
    in
    let layout =
      let open Snips.Infix in
      Snips.top which_api_view
      |+| Snips.right params_view
      |+| Snips.body ~attr:{%css|margin-bottom: 10px;|} view
    in
    Snips.render
      ~container_attr:(Vdom.Attr.many [ controls; {%css|margin-left: 10px;|} ])
      layout
  in
  (* DANGER! Do NOT write this in real app code, use [match%sub] instead. *)
  Bonsai.Let_syntax.Let_syntax.switch
    ~here:[%here]
    ~match_
    ~branches:(List.length all_configs)
    ~with_ [@nontail]
;;

let () =
  component
  |> View.Theme.set_for_app (Bonsai.return (Kado.theme ~version:Bleeding ()))
  |> Bonsai_web.Start.start
       ~use_new_experimental_implementation:true
       ~enable_bonsai_telemetry:Enabled
;;
