open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Gallery = Bonsai_web_ui_gallery

(* These tests are intended for app writers.
   There are additional demos in [toplayer_testing], which are lower-level, and intended
   for toplayer devs to test changes. *)

let component (local_ graph) =
  let%sub theme, theme_picker = Gallery.Theme_picker.component () graph in
  View.Theme.set_for_app
    theme
    (Gallery.make_sections
       ~theme_picker
       [ ( "Tooltip"
         , {|Tooltips appear next to an element when it is hovered.|}
         , let open Tooltip in
           [ Gallery.make_demo (module Tooltip)
           ; Gallery.make_demo (module Tooltip_positioning)
           ; Gallery.make_demo (module Tooltip_with_arbitrary_content)
           ] )
       ; ( "Popovers"
         , {| Popovers are like more powerful tooltips, in that they can contain Bonsai
              state / computations. |}
         , let open Popover in
           [ Gallery.make_demo (module Popover)
           ; Gallery.make_demo (module Nested_popover)
           ; Gallery.make_demo (module Popover_with_arrow)
           ; Gallery.make_demo (module Popover_close_on_click)
           ] )
       ; ( "Modals"
         , {| Modals are like popovers, but they make the rest of the web app inert.
              They also only support css-based positioning. |}
         , let open Modal in
           [ Gallery.make_demo (module Modal)
           ; Gallery.make_demo (module Modal_lock_body_scroll)
           ; Gallery.make_demo (module Nested_modals)
           ; Gallery.make_demo (module Modal_and_popover_interactions)
           ] )
       ])
    graph
;;

let () =
  Async_js.init ();
  Bonsai_web.Start.start component ~enable_bonsai_telemetry:Enabled
;;
