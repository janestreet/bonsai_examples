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
            ~overflow_auto_wrapper:(Bonsai.return false)
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
            ~overflow_auto_wrapper:(Bonsai.return false)
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
                  ~overflow_auto_wrapper:(Bonsai.return false)
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
            ~overflow_auto_wrapper:(Bonsai.return false)
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
            ~overflow_auto_wrapper:(Bonsai.return false)
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
                  ~overflow_auto_wrapper:(Bonsai.return false)
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
                   | true -> Vdom.Node.none
                   | false -> View.button theme ~on_click:open_outer "Open Outer Popover")
                ; View.button theme ~on_click:close "Close Modal"
                ])
            ~overflow_auto_wrapper:(Bonsai.return true)
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
