open! Core
open! Bonsai_web
open Bonsai.Let_syntax

let component (local_ graph) =
  let%sub { tag; reset = reset_tag } = Tag.component graph in
  let%sub { attr; reset = reset_attr } = Attr.component graph in
  let%sub { out = before_state; view = before_view; reset = reset_before } =
    Color_list.component "before" graph
  in
  let%sub { out = after_state; view = after_view; reset = reset_after } =
    Color_list.component "after" graph
  in
  let%sub { state; view = tweener; is_automating = is_running; is_done; step } =
    Stepper.component ~before_state ~after_state graph
  in
  let reset_all =
    let%arr reset_before and reset_after and reset_tag and reset_attr in
    Effect.Many [ reset_before; reset_after; reset_tag; reset_attr ]
  in
  let%sub () = Automator.component ~is_running ~step ~is_done ~reset_all graph in
  let comparison = Comparison.view ~tag ~attr state graph in
  let%arr before_view and after_view and tweener and comparison in
  Vdom.Node.div ~attrs:[ Style.app ] [ before_view; after_view; tweener; comparison ]
;;

let () = Bonsai_web.Start.start component
