open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

let component (local_ graph) =
  let docs = Bonsai_garden_ppx_html_docs.component graph in
  let%arr docs in
  {%html|<div style="height: calc(100vh - 16px)">%{docs}</div>|}
;;

let () = Bonsai_web.Start.start component ~enable_bonsai_telemetry:Enabled
