open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Gallery = Bonsai_web_ui_gallery
module Toplayer = Bonsai_web_ui_toplayer

module Modal = struct
  let name = "Modal"

  let description =
    {|Modals block content behind them. By default, modals close on clicks and presses of "escape",
     but not on right clicks. |}
  ;;

  let view (local_ graph) =
    let computation, demo =
      [%demo
        let { Toplayer.Controls.open_; _ } =
          Toplayer.Modal.create
            ~content:(fun ~close (local_ graph) ->
              let%arr theme = View.Theme.current graph
              and close in
              View.vbox
                [ View.text "Hi, I am a modal"
                ; View.button theme ~on_click:close "Close Modal"
                ])
            graph
        in
        let%arr theme = View.Theme.current graph
        and open_ in
        View.button theme ~intent:Info ~on_click:open_ "Open Modal"]
    in
    let%arr computation in
    computation, demo
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Modal_lock_body_scroll = struct
  let name = "Modal + Unscrollable Body"

  let description =
    {| If [lock_body_scroll] is true, the body can't be scrolled while
  the modal is open. This might cause a slight layout shift due to the scrollbar disappearing.
  You can set the `scrollbar-gutter` CSS style on your app to always reserve space for the
  scrollbar. |}
  ;;

  let view (local_ graph) =
    let computation, demo =
      [%demo
        let { Toplayer.Controls.open_; _ } =
          Toplayer.Modal.create
            ~lock_body_scroll:(return true)
            ~content:(fun ~close:_ (local_ _graph) ->
              View.text "Hi, I am a modal" |> return)
            graph
        in
        let%arr theme = View.Theme.current graph
        and open_ in
        View.button theme ~intent:Info ~on_click:open_ "Open Modal"]
    in
    let%arr computation in
    computation, demo
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Nested_modals = struct
  let name = "Nested Modals"
  let description = {|You can put modals into other modals! |}

  let view (local_ graph) =
    let computation, demo =
      [%demo
        let { Toplayer.Controls.open_; _ } =
          Toplayer.Modal.create
            ~content:(fun ~close (local_ graph) ->
              let { Toplayer.Controls.open_ = open_inner; _ } =
                Toplayer.Modal.create
                  ~content:(fun ~close (local_ graph) ->
                    let%arr theme = View.Theme.current graph
                    and close in
                    View.vbox
                      [ View.text "Hi, I am a modal"
                      ; View.button theme ~on_click:close "Close Modal"
                      ])
                  graph
              in
              let%arr theme = View.Theme.current graph
              and close
              and open_inner in
              View.vbox
                [ View.text "Hi, I am a modal"
                ; View.button theme ~on_click:open_inner "Open Inner Modal"
                ; View.button theme ~on_click:close "Close Modal"
                ])
            graph
        in
        let%arr theme = View.Theme.current graph
        and open_ in
        View.button theme ~intent:Info ~on_click:open_ "Open Modal"]
    in
    let%arr computation in
    computation, demo
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Modal_and_popover_interactions = struct
  let name = "Modals + Popovers"

  let description =
    {| Modals and popovers will be stacked by whichever was opened first.
    Popovers under a modal will be inert.
    Popovers inside of an open modal are exempt from inertness. |}
  ;;

  let view (local_ graph) =
    let computation, demo =
      [%demo
        let ( popover_attr
            , { Toplayer.Controls.open_ = open_popover; is_open = outer_is_open; _ } )
          =
          Toplayer.Popover.create
            ~position:(return Toplayer.Position.Right)
            ~close_on_click_outside:(return Toplayer.Close_on_click_outside.No)
            ~content:(fun ~close (local_ graph) ->
              let%arr theme = View.Theme.current graph
              and close in
              View.vbox
                [ View.text "Hi, I am a popover"
                ; View.button theme ~on_click:close "Close Popover"
                ])
            graph
        in
        let { Toplayer.Controls.open_ = open_modal; _ } =
          Toplayer.Modal.create
            ~content:(fun ~close (local_ graph) ->
              let open_outer = open_popover in
              let popover_attr, { Toplayer.Controls.open_ = open_popover; _ } =
                Toplayer.Popover.create
                  ~content:(fun ~close (local_ graph) ->
                    let%arr theme = View.Theme.current graph
                    and close in
                    View.vbox
                      [ View.text "Inner Popover"
                      ; View.button theme ~on_click:close "Close Popover"
                      ])
                  graph
              in
              let%arr theme = View.Theme.current graph
              and open_popover
              and popover_attr
              and close
              and open_outer
              and outer_is_open in
              View.vbox
                [ View.text "Hi, I am a modal"
                ; View.button
                    ~attrs:[ popover_attr ]
                    theme
                    ~on_click:open_popover
                    "Open Inner Popover"
                ; (match outer_is_open with
                   | true -> Vdom.Node.none_deprecated [@alert "-deprecated"]
                   | false -> View.button theme ~on_click:open_outer "Open Outer Popover")
                ; View.button theme ~on_click:close "Close Modal"
                ])
            graph
        in
        let%arr theme = View.Theme.current graph
        and open_modal
        and open_popover
        and popover_attr in
        View.hbox
          [ View.button theme ~intent:Info ~on_click:open_modal "Open Modal"
          ; View.button
              theme
              ~intent:Info
              ~attrs:[ popover_attr ]
              ~on_click:open_popover
              "Open Popover"
          ]]
    in
    let%arr computation in
    computation, demo
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Old_modal = struct
  let name = "Modal"
  let description = {|  |}

  let view (local_ graph) =
    let vdom, demo =
      [%demo
        let modal_1_contents (local_ _graph) =
          Bonsai.return (Vdom.Node.div [ Vdom.Node.text "Surprise!" ])
        in
        let modal_2_contents n (local_ _graph) =
          let got_ya =
            match%arr n with
            | 1 -> "Got ya!"
            | n -> sprintf "Got ya %d times!" n
          in
          let%arr got_ya in
          Vdom.Node.div
            [ Vdom.Node.text "Surprise!"; Vdom.Node.br (); Vdom.Node.text got_ya ]
        in
        let modal_1 =
          Bonsai_web_ui_modal.create
            (module Unit)
            (fun _ ~hide_self:_ -> modal_1_contents)
            ~equal:[%equal: Unit.t]
            graph
        in
        let modal_2 =
          Bonsai_web_ui_modal.create
            (module Int)
            (fun n ~hide_self:_ -> modal_2_contents n)
            ~equal:[%equal: Int.t]
            graph
        in
        let state, set_state =
          Bonsai.state 1 ~sexp_of_model:[%sexp_of: Int.t] ~equal:[%equal: Int.t] graph
        in
        let%arr state and set_state and modal_1 and modal_2 in
        Vdom.Node.div
          [ Vdom.Node.button
              ~attrs:[ Vdom.Attr.on_click (fun _ -> modal_1.show ()) ]
              [ Vdom.Node.text "Click me!" ]
          ; modal_1.view
          ; Vdom.Node.br ()
          ; Vdom.Node.button
              ~attrs:
                [ Vdom.Attr.on_click (fun _ ->
                    let%bind.Effect () = set_state (state + 1) in
                    modal_2.show state)
                ; {%css|margin-top: 10px;|}
                ]
              [ Vdom.Node.text "Click me multiple times!" ]
          ; modal_2.view
          ]]
    in
    Bonsai.map vdom ~f:(fun vdom -> vdom, demo)
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

let component (local_ graph) =
  let%sub theme, theme_picker = Gallery.Theme_picker.component () graph in
  View.Theme.set_for_app
    theme
    (Gallery.make_sections
       ~theme_picker
       [ ( "Bonsai_web_ui_toplayer"
         , {| Modals are like popovers, but they make the rest of the web UI inert. |}
         , [ Gallery.make_demo (module Modal)
           ; Gallery.make_demo (module Modal_lock_body_scroll)
           ; Gallery.make_demo (module Nested_modals)
           ; Gallery.make_demo (module Modal_and_popover_interactions)
           ] )
       ; ( "Bonsai_web_ui_modal"
         , {|  `Bonsai_web_ui_modal` is an older implementation of modals; new apps should
         use the toplayer version instead. |}
         , [ Gallery.make_demo (module Old_modal) ] )
       ])
    graph
;;

let () = Bonsai_web.Start.start component
