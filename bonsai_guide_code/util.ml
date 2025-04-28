open! Core
open! Bonsai_web
open Js_of_ocaml

let () = Async_js.init ()
let all_hashes = ref String.Set.empty
let valid_hash_provided = ref false

let run ?custom_connector ~id computation =
  all_hashes := Set.add !all_hashes id;
  (* Because we're iframing into this app from docpub, we look up what the
     current url-hash is, and only run the requested example. *)
  let current_hash = Dom_html.window##.location##.hash |> Js.to_string in
  print_s [%message (current_hash : string) (id : string)];
  if String.equal current_hash ("#" ^ id)
  then (
    valid_hash_provided := true;
    Start.start ?custom_connector computation ~enable_bonsai_telemetry:Enabled)
  else ()
;;

let run_vdom_val vdom = run (fun _ -> vdom)
let run_vdom vdom = run (fun _ -> Bonsai.return vdom)

let no_hash_fallback_must_run_last () =
  let current_hash = Dom_html.window##.location##.hash |> Js.to_string in
  if not !valid_hash_provided
  then
    Start.start
      (fun _ ->
        Bonsai.return
          (let example_urls =
             Set.to_list !all_hashes
             |> List.map ~f:(fun id ->
               let url = Uri.make ~fragment:id () |> Uri.to_string in
               let on_click _ =
                 let%bind.Effect () = Effect.open_url ~in_:This_tab url in
                 Dom_html.window##.location##reload;
                 Effect.Ignore
               in
               {%html|
                 <li>
                   <a href=%{url} on_click=%{on_click}>#{id}</a>
                 </li>
               |})
           in
           {%html|
             <div>
               <h1>Invalid URL</h1>
               <p>
                 To use the Bonsai guide demos, please provide one of the example IDs as the
                 hash in the URL.
               </p>
               <p>Provided hash: #{current_hash}</p>
               <ul>
                 *{example_urls}
               </ul>
             </div>
           |}))
      ~enable_bonsai_telemetry:Enabled
;;
