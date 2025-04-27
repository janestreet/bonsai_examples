open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

(* $MDX part-begin=view_unthemed *)

let text_a = Vdom.Node.span [ Vdom.Node.text "A" ]
let text_b = Vdom.Node.span [ Vdom.Node.text "B" ]
let text_c = Vdom.Node.span [ Vdom.Node.text "C" ]

let flex_container =
  Vdom.Node.div
    ~attrs:
      [ {%css|
          flex-direction: row;
          flex-wrap: wrap;
        |}
      ]
    [ text_a; text_b; text_c ]
;;

(* vs *)

let text_a', text_b', text_c' = View.text "A", View.text "B", View.text "C"
let flex_container' = View.hbox_wrap [ text_a'; text_b'; text_c' ]
(* $MDX part-end *)

let () = ignore flex_container
let () = Util.run_vdom flex_container' ~id:"view_hbox"
let do_thing = Effect.alert "Did it!"
let theme = View.Expert.default_theme

(* $MDX part-begin=view_button *)
open Virtual_dom

let button =
  Vdom.Node.button
    ~attrs:[ Vdom.Attr.on_click (fun _ -> do_thing) ]
    [ Vdom.Node.text "click me" ]
;;

(* vs *)

let button' = View.button theme ~on_click:do_thing "click me"
(* $MDX part-end *)

let () = ignore button
let () = Util.run_vdom button' ~id:"view_button"

(* $MDX part-begin=button_get_theme *)
let error_button (local_ graph) =
  let theme = View.Theme.current graph in
  let%arr theme in
  View.button theme ~intent:Error ~on_click:do_thing "Error button"
;;

(* $MDX part-end *)

let () = Util.run error_button ~id:"button_get_theme"

(* $MDX part-begin=set_theme *)
let app (local_ graph) =
  View.Theme.set_for_app (Kado.theme ~version:V1 () |> Bonsai.return) error_button graph
;;

(* $MDX part-end *)

let () = Util.run app ~id:"set_theme"

(* $MDX part-begin=theme_toggle *)
let themed_theme_toggler ~toggle_dark (local_ graph) =
  let%arr theme = View.Theme.current graph
  and toggle_dark
  and error_button = error_button graph in
  View.hbox [ View.button theme ~on_click:toggle_dark "Toggle Dark Mode"; error_button ]
;;

let app (local_ graph) =
  let theme_style, set_theme_style = Bonsai.state Kado.Style.Dark graph in
  let theme =
    let%arr theme_style in
    Kado.theme ~style:theme_style ~version:V1 ()
  in
  let toggle_dark =
    let%arr theme_style and set_theme_style in
    set_theme_style
      (match theme_style with
       | Dark -> Light
       | Light -> Dark)
  in
  View.Theme.set_for_app theme (themed_theme_toggler ~toggle_dark) graph
;;

(* $MDX part-end *)

let () = Util.run app ~id:"theme_toggle"
let () = Util.run app ~id:"set_theme"

(* $MDX part-begin=override_constants *)
let app (local_ graph) =
  View.Theme.override_constants_for_computation
    error_button
    ~f:(fun constants ->
      { constants with
        intent =
          { constants.intent with
            error = { background = `Hex "#4c0121"; foreground = `Hex "#e39ff6" }
          }
      })
    graph
;;

(* $MDX part-end *)

let () = Util.run app ~id:"override_constants"

(* $MDX part-begin=override_theme *)
let app (local_ graph) =
  View.Expert.override_theme_for_computation
    error_button
    ~f:(fun (module S) ->
      (module struct
        class c =
          object
            inherit S.c

            method! button ~attrs:_ ~disabled:_ ~intent:_ ~tooltip:_ ~on_click:_ _content
                =
              Vdom.Node.text "Buttons are banned by the Great Convention."
          end
      end))
    graph
;;

(* $MDX part-end *)

let () = Util.run app ~id:"override_theme"
