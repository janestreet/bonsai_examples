open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_manual_view
module Cm_ro = Bonsai_web_ui_codemirror_read_only

let settings (local_ graph) =
  let lines_count_form = Form.Elements.Number.int ~default:1000 ~step:1 () graph in
  let print_full_document_form = Form.Elements.Checkbox.bool ~default:false () graph in
  let view =
    let%arr lines_count_form and print_full_document_form in
    View.vbox
      [ View.hbox [ View.text "Lines count: "; Form.view lines_count_form ]
      ; View.hbox
          [ View.text "Print full document: "; Form.view print_full_document_form ]
      ]
  in
  let text =
    let%arr lines_count_form in
    let lines_count = Form.value_or_default lines_count_form ~default:1000 in
    String.concat_lines (List.init lines_count ~f:(fun i -> Int.to_string i))
  in
  let print_full_document =
    let%arr print_full_document_form in
    Form.value_or_default print_full_document_form ~default:false
  in
  text, print_full_document, view
;;

let component graph =
  let text, print_full_document, settings_view = settings graph in
  let editor_view =
    let%arr text and print_full_document in
    Cm_ro.make ~print_full_document ~language:Sexp ~theme:Basic_dark text
  in
  let%arr settings_view and editor_view in
  View.vbox [ settings_view; editor_view ]
;;

let () = Bonsai_web.Start.start component ~enable_bonsai_telemetry:Enabled
