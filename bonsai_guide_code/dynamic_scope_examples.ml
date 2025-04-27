open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

(* $MDX part-begin=theme_dynamic_scope *)
module Theme = struct
  type t =
    | Light
    | Dark

  let styles = function
    | Light ->
      {%css|
        background-color: black;
        color: white;
      |}
    | Dark ->
      {%css|
        background-color: white;
        color: black;
      |}
  ;;
end

let theme = Bonsai.Dynamic_scope.create ~name:"theme" ~fallback:Theme.Dark ()

let thing_1 (local_ graph) =
  let%arr theme = Bonsai.Dynamic_scope.lookup theme graph in
  Vdom.Node.div ~attrs:[ Theme.styles theme ] [ Vdom.Node.text "Thing 1" ]
;;

let thing_2 (local_ graph) =
  let%arr theme = Bonsai.Dynamic_scope.lookup theme graph in
  Vdom.Node.div
    [ Vdom.Node.button ~attrs:[ Theme.styles theme ] [ Vdom.Node.text "Thing 2" ] ]
;;

let app (local_ graph) =
  let%arr thing_1 = thing_1 graph
  and thing_2 = thing_2 graph in
  Vdom.Node.div [ thing_1; thing_2 ]
;;

(* $MDX part-end *)

let () = Util.run app ~id:"theme_dynamic_scope"

(* $MDX part-begin=theme_set *)

let app (local_ graph) =
  Bonsai.Dynamic_scope.set theme (Bonsai.return Theme.Light) ~inside:app graph
;;

(* $MDX part-end *)

let () = Util.run app ~id:"theme_set"
