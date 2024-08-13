open! Core
open! Async
open Bonsai_examples_mouse_position_common

let initialize_connection user _initiated_from _addr connection =
  print_s [%message "user joined" (user : Krb.Principal.Name.t)];
  { Connection_state.user = Krb.Principal.Name.to_username_exn user
  ; session = Session.create ()
  ; connection
  }
;;

let main ~http_settings ~js_file =
  let%bind server =
    let open Cohttp_static_handler in
    let javascript =
      Asset.local
        Asset.Kind.javascript
        (Asset.What_to_serve.file ~path:js_file ~relative_to:`Cwd)
    in
    let sourcemap_file = String.chop_suffix_exn js_file ~suffix:".js" ^ ".map" in
    let sourcemap =
      Asset.local
        Asset.Kind.sourcemap
        (Asset.What_to_serve.file ~path:sourcemap_file ~relative_to:`Cwd)
    in
    let http_handler _principle =
      Single_page_handler.create_handler
        (Single_page_handler.default_with_body_div ~div_id:"app")
        ~assets:[ javascript; sourcemap ]
        ~on_unknown_url:`Not_found
    in
    let csp =
      Csp_monoid.reduce
        [ Csp_monoid.default_for_clientside
        ; Csp_monoid.frame_ancestor "https://localhost:*"
          (* allow to be iframed in localhost addresses like for local dev *)
        ; Csp_monoid.frame_ancestor "https://bonsai:*"
          (* allow to be iframed in https://bonsai/ *)
        ]
      |> Csp_monoid.finalize
    in
    Simple_web_server.create
      ~authorize:Krb_http.Authorize.accept_all
      ~content_security_policy:(`Block csp)
      ~rpc_config:
        (Simple_web_server.Rpc_config.create
           ~implementations:
             (Rpc.Implementations.create_exn
                ~implementations:
                  (Rpc_implementations.create
                     ~on_client_and_server_out_of_sync:Log.Global.info_s)
                ~on_unknown_rpc:`Continue
                ~on_exception:Log_on_background_exn)
           ~initial_connection_state:initialize_connection)
      http_settings
      http_handler
  in
  let server = Or_error.ok_exn server in
  Simple_web_server.close_finished server
;;

let command =
  Command.async
    ~behave_nicely_in_pipeline:true
    ~summary:"Start server for the mouse positions example"
    (let%map_open.Command http_settings = Http_settings.param ()
     and js_file =
       flag
         "js-file"
         (required Filename_unix.arg_type)
         ~doc:"FILENAME The path to the JavaScript file which is served by the web server"
     in
     fun () -> main ~http_settings ~js_file)
;;

let command_for_bonsai_example_server =
  Command.async
    ~behave_nicely_in_pipeline:true
    ~summary:""
    (let%map_open.Command http_settings = Http_settings.param () in
     fun () ->
       let open Exe_server_protocol.Types in
       Tempfile.with_tempdir ~prefix:"mouse_position" ~suffix:"" (fun tempdir ->
         let js_file = [%string "%{tempdir}/mouse_position.js"] in
         let%bind () =
           Exe_server_client.Exe_client.download_file
             ~remote:
               Path.(
                 empty /^ "prod" /^ "bonsai" /^ "examples-server" /^ "mouse_position.js")
             ~local:js_file
             ~file_cache:`Default
             ()
           >>| ok_exn
         in
         main ~http_settings ~js_file))
;;
