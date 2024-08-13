open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Url_var = Bonsai_web_ui_url_var
module Form = Bonsai_web_ui_form.With_automatic_view
open Uri_parsing

module Location = struct
  type t =
    { x : int
    ; y : int
    }
  [@@deriving typed_fields, sexp, equal, compare]

  let parser_for_field : type a. a Typed_field.t -> a Parser.t =
    let open Parser in
    let open Value_parser in
    function
    | X -> from_query_required int
    | Y -> from_query_optional_with_default ~equal:Int.equal int ~default:100
  ;;

  let form_for_field : type a. a Typed_field.t -> Bonsai.graph -> a Form.t Bonsai.t =
    let open Form.Elements.Textbox in
    function
    | X -> int ~allow_updates_when_focused:`Never ()
    | Y -> int ~allow_updates_when_focused:`Never ()
  ;;

  let form_of_t : Bonsai.graph -> t Form.t Bonsai.t =
    Form.Typed.Record.make
      (module struct
        module Typed_field = Typed_field

        let form_for_field = form_for_field
        let label_for_field = `Inferred
      end)
  ;;

  module Path_order = Path_order (Typed_field)

  let path_order = Path_order.(T [])
end

module Record = struct
  type t =
    { an_int : int
    ; many_floats : float list
    ; optional_string : string option
    ; many_locations : Location.t list
    ; nested : Location.t
    ; username_on_path : string
    ; comment_id_on_path : int
    ; remaining_words_on_path : string list
    }
  [@@deriving typed_fields, sexp, equal, compare]

  let parser_for_field : type a. a Typed_field.t -> a Parser.t =
    let open Parser in
    let open Value_parser in
    function
    | An_int -> from_query_required int
    | Many_floats -> from_query_many float
    | Optional_string -> from_query_optional string
    | Many_locations -> from_query_many (sexpable (module Location))
    | Nested -> Parser.Record.make (module Location)
    | Username_on_path -> with_prefix [ "username" ] (from_path string)
    | Comment_id_on_path -> with_prefix [ "id" ] (from_path int)
    | Remaining_words_on_path -> with_prefix [] (from_remaining_path string)
  ;;

  let form_of_t : Bonsai.graph -> t Form.t Bonsai.t =
    fun graph ->
    Form.Typed.Record.make
      (module struct
        module Typed_field = Typed_field

        let form_for_field : type a. a Typed_field.t -> Bonsai.graph -> a Form.t Bonsai.t =
          fun typed_field graph ->
          let open Form.Elements.Textbox in
          let open Form.Elements in
          match typed_field with
          | An_int -> int ~allow_updates_when_focused:`Never () graph
          | Many_floats ->
            Multiple.list (float ~allow_updates_when_focused:`Never ()) graph
          | Optional_string ->
            let form = string ~allow_updates_when_focused:`Never () graph in
            let%arr form = form in
            Form.optional form ~is_some:(fun x -> not (String.equal x "")) ~none:""
          | Many_locations -> Multiple.list Location.form_of_t graph
          | Nested -> Location.form_of_t graph
          | Username_on_path -> string ~allow_updates_when_focused:`Never () graph
          | Comment_id_on_path -> int ~allow_updates_when_focused:`Never () graph
          | Remaining_words_on_path ->
            Multiple.list (string ~allow_updates_when_focused:`Never ()) graph
        ;;

        let label_for_field = `Inferred
      end)
      graph
  ;;

  module Path_order = Path_order (Typed_field)

  let path_order =
    Path_order.(T [ Username_on_path; Comment_id_on_path; Remaining_words_on_path ])
  ;;
end

module Variant = struct
  type t =
    | Post
    | Comments
  [@@deriving typed_variants, sexp, compare, equal]

  let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
    | Post -> Parser.unit
    | Comments -> Parser.unit
  ;;

  let form_of_t =
    Form.Typed.Variant.make
      (module struct
        module Typed_variant = Typed_variant

        let form_for_variant
          : type a. a Typed_variant.t -> Bonsai.graph -> a Form.t Bonsai.t
          =
          fun typed_field _graph ->
          match typed_field with
          | Post -> Bonsai.return (Form.return ())
          | Comments -> Bonsai.return (Form.return ())
        ;;

        let label_for_variant = `Inferred
        let initial_choice = `First_constructor
      end)
  ;;
end

module Query_variant = struct
  type t =
    | A of int
    | B of float
    | C of string
  [@@deriving sexp, equal, compare, typed_variants]

  let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
    | A -> Parser.from_query_required Value_parser.int
    | B -> Parser.from_query_required Value_parser.float
    | C -> Parser.from_query_required Value_parser.string
  ;;

  let identifier_for_variant = Typed_variant.name

  let form_of_t =
    Form.Typed.Variant.make
      (module struct
        module Typed_variant = Typed_variant

        let form_for_variant
          : type a. a Typed_variant.t -> Bonsai.graph -> a Form.t Bonsai.t
          =
          fun typed_field graph ->
          match typed_field with
          | A -> Form.Elements.Textbox.int ~allow_updates_when_focused:`Never () graph
          | B -> Form.Elements.Textbox.float ~allow_updates_when_focused:`Never () graph
          | C -> Form.Elements.Textbox.string ~allow_updates_when_focused:`Never () graph
        ;;

        let label_for_variant = `Inferred
        let initial_choice = `First_constructor
      end)
  ;;
end

module T = struct
  type t =
    | Homepage
    | Some_string_option of string option
    | Variant of Variant.t
    | Query_variant of Query_variant.t
    | Record of Record.t
    | Unable_to_parse
  [@@deriving sexp, equal, compare, typed_variants]

  let parser_for_variant : type a. a Typed_variant.t -> a Parser.t = function
    | Homepage -> Parser.with_remaining_path [] Parser.unit
    | Some_string_option -> Parser.from_query_optional ~key:"extra" Value_parser.string
    | Variant -> Parser.Variant.make (module Variant)
    | Record -> Parser.Record.make (module Record)
    | Query_variant -> Parser.Query_based_variant.make ~key:"page" (module Query_variant)
    | Unable_to_parse -> Parser.with_remaining_path [ "unable" ] Parser.unit
  ;;

  let form_of_t graph =
    Form.Typed.Variant.make
      (module struct
        module Typed_variant = Typed_variant

        let form_for_variant
          : type a. a Typed_variant.t -> Bonsai.graph -> a Form.t Bonsai.t
          =
          fun typed_field graph ->
          match typed_field with
          | Homepage -> Bonsai.return (Form.return ())
          | Some_string_option ->
            let text =
              Form.Elements.Textbox.string ~allow_updates_when_focused:`Never () graph
            in
            let%arr text = text in
            Form.optional
              text
              ~is_some:(function
                | "" -> false
                | _ -> true)
              ~none:""
          | Query_variant -> Query_variant.form_of_t graph
          | Variant -> Variant.form_of_t graph
          | Record -> Record.form_of_t graph
          | Unable_to_parse -> Bonsai.return (Form.return ())
        ;;

        let label_for_variant = `Inferred
        let initial_choice = `First_constructor
      end)
      graph
  ;;
end

let parser = Parser.Variant.make (module T)
let versioned_parser = Url_var.Typed.Versioned_parser.first_parser parser

let%expect_test _ =
  Parser.check_ok_and_print_urls_or_errors parser;
  [%expect
    {|
    URL parser looks good!
    ┌──────────────────────────────────────────────────────────────────────────────────────────┐
    │ All urls                                                                                 │
    ├──────────────────────────────────────────────────────────────────────────────────────────┤
    │ /                                                                                        │
    │ /query_variant?page=a&query_variant.a=<int>                                              │
    │ /query_variant?page=b&query_variant.b=<float>                                            │
    │ /query_variant?page=c&query_variant.c=<string>                                           │
    │ /some_string_option?extra=<optional<string>>                                             │
    │ /unable                                                                                  │
    │ /username/<string>/id/<int>/<multiple<string>>?record.an_int=<int>&record.many_floats=<m │
    │ ultiple<float>>&record.many_locations=<multiple<sexpable>>&record.nested.x=<int>&record. │
    │ nested.y=<optional<int>>&record.optional_string=<optional<string>>                       │
    │ /variant/comments                                                                        │
    │ /variant/post                                                                            │
    └──────────────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;

let fallback _exn _components = T.Unable_to_parse

let component ~url_var graph =
  let url_value = Url_var.value url_var in
  let modify_history, toggle_modify_history = Bonsai.toggle ~default_model:true graph in
  let set_url_var =
    let%arr modify_history = modify_history in
    let how = if modify_history then `Push else `Replace in
    fun query -> Url_var.set_effect url_var query ~how
  in
  let form = T.form_of_t graph in
  let store_value =
    let%arr url_value = url_value in
    Some url_value
  in
  let%sub () =
    Form.Dynamic.sync_with
      ~sexp_of_model:[%sexp_of: T.t]
      ~equal:[%equal: T.t]
      ~store_value
      ~store_set:set_url_var
      form
      graph
  in
  let update_button =
    let update_effect =
      let%arr modify_history = modify_history in
      let how = if modify_history then `Push else `Replace in
      fun ~f -> Url_var.update_effect url_var ~how ~f
    in
    let%arr update_effect = update_effect in
    Vdom.Node.button
      ~attrs:[ Vdom.Attr.on_click (fun _ -> update_effect ~f:(fun _ -> Homepage)) ]
      [ Vdom.Node.text "Go to homepage via [update]" ]
  in
  let%arr form = form
  and url = url_value
  and modify_history = modify_history
  and toggle_modify_history = toggle_modify_history
  and update_button = update_button in
  let saved_to_history_text =
    if modify_history
    then "History is being saved in your browser"
    else "History is not being saved in your browser"
  in
  View.vbox
    [ Vdom.Node.text
        "Change form to update the URL's query! Change the URL's query to update the \
         form! (Errors are detected and routed to the fallback page!)"
    ; View.hbox [ Form.view_as_vdom form ]
    ; Vdom.Node.p
        [ Vdom.Node.text saved_to_history_text
        ; Vdom.Node.br ()
        ; Vdom.Node.button
            ~attrs:[ Vdom.Attr.on_click (fun _ -> toggle_modify_history) ]
            [ Vdom.Node.text "Toggle history saving" ]
        ]
    ; View.hbox [ update_button ]
    ; Vdom.Node.text "Here's what the parsed query as a sexp looks like:"
    ; Vdom.Node.code [ Vdom.Node.sexp_for_debugging (T.sexp_of_t url) ]
    ]
;;
