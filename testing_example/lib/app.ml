open! Core
open! Bonsai_web
open Bonsai.Let_syntax

(* $MDX part-begin=hello-world-component *)
let hello_world : local_ Bonsai.graph -> Vdom.Node.t Bonsai.t =
  fun (local_ _graph) -> Bonsai.return (Vdom.Node.span [ Vdom.Node.text "hello world" ])
;;

(* $MDX part-end *)

(* $MDX part-begin=hello-user-component *)
let hello_user (name : string Bonsai.t) : local_ Bonsai.graph -> Vdom.Node.t Bonsai.t =
  fun (local_ _graph) ->
  let%arr name in
  Vdom.Node.span [ Vdom.Node.textf "hello %s" name ]
;;

(* $MDX part-end *)

(* $MDX part-begin=hello-text-box-component *)
let hello_textbox : local_ Bonsai.graph -> Vdom.Node.t Bonsai.t =
  fun (local_ graph) ->
  let state, set =
    Bonsai.state "" ~sexp_of_model:[%sexp_of: String.t] ~equal:[%equal: String.t] graph
  in
  let message = hello_user state graph in
  let%arr message and set in
  Vdom.Node.div
    [ Vdom.Node.input ~attrs:[ Vdom.Attr.on_input (fun _ text -> set text) ] (); message ]
;;
(* $MDX part-end *)
