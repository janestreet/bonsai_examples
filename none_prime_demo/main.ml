open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

module Style =
  [%css
  stylesheet
    {|
      .box {
        width: 50px;
        height: 50px;
        border: 3px solid black;
        border-radius: 5px;

        display: flex;
        align-items: center;
        justify-content: center;

        font-family: monospace;
      }
    |}]

module Flashing_box_attr = Vdom.Attr.Hooks.Make (struct
    open Js_of_ocaml
    module State = Unit

    module Input = struct
      type t = string [@@deriving sexp_of]

      let combine _ r = r
    end

    let on_mount = `Do_nothing

    let destroy _ () element =
      element##.style##.borderColor := Js.string "";
      element##.style##.background := Js.string ""
    ;;

    let flash element =
      element##.style##.borderColor := Js.string "red";
      element##.style##.background := Js.string "pink";
      let _ : Dom_html.timeout_id_safe =
        Dom_html.setTimeout (fun () -> destroy () () element) 250.0
      in
      ()
    ;;

    let init string (element : Dom_html.element Js.t) =
      element##.innerText := Js.string string;
      flash element
    ;;

    let update ~old_input ~new_input () (element : Dom_html.element Js.t) =
      if String.equal old_input new_input
      then ()
      else (
        element##.innerText := Js.string new_input;
        flash element)
    ;;
  end)

let hook text = Vdom.Attr.create_hook "flashing-box" (Flashing_box_attr.create text)
let box text = Vdom.Node.div ~attrs:[ Style.box; hook text ] []

let component (local_ graph) =
  let state, toggle = Bonsai.toggle ~default_model:false graph in
  let%arr state
  and toggle
  and theme = View.Theme.current graph in
  View.vbox
    [ Vdom.Node.p
        [ Vdom.Node.text {| Click the button to toggle the 'c' element on and off. |} ]
    ; Vdom.Node.div [ View.button theme ~on_click:toggle "toggle 'c'" ]
    ; Vdom.Node.p
        [ Vdom.Node.text {| Red flashing indicates a vdom diff on that element. |} ]
    ; View.hbox
        ~gap:(`Em 1)
        [ View.vbox
            ~gap:(`Em 1)
            [ View.themed_text theme ~size:Large "none_deprecated"
            ; box "a"
            ; box "b"
            ; (if state
               then
                 (* NOTE: The purpose of this demo is to compare the previous none
                    function with the new one so we are intentionally using the deprecated
                    function. *)
                 Vdom.Node.none_deprecated
                 [@alert "-deprecated"]
               else box "c")
            ; box "d"
            ; box "e"
            ]
        ; View.vbox
            ~gap:(`Em 1)
            [ View.themed_text theme ~size:Large "none"
            ; box "a"
            ; box "b"
            ; (if state then Vdom.Node.none else box "c")
            ; box "d"
            ; box "e"
            ]
        ]
    ]
;;

let () = Bonsai_web.Start.start component ~enable_bonsai_telemetry:Enabled
