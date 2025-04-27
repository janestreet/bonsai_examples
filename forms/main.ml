open! Core
open! Bonsai_web

module Style =
  [%css
  stylesheet
    {|
      .container {
        display: flex;
        flex-flow: row nowrap;
        outline: none;
      }

      .container > div {
        padding: 20px;
      }
    |}]

let component (local_ graph) =
  let%map.Bonsai big_form = Big_form.component graph
  and list_form = List_form.component graph
  and form_with_submit = Form_with_submit.component graph
  and typed_record = Typed.component graph
  and file_form = File_form.form graph
  and custom_form = Custom_form.component graph in
  Vdom.Node.div
    ~attrs:[ Style.container ]
    [ big_form; list_form; form_with_submit; typed_record; file_form; custom_form ]
;;

let () = Bonsai_web.Start.start component ~enable_bonsai_telemetry:Enabled
