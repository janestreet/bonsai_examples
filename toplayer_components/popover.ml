open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Gallery = Bonsai_web_ui_gallery
module Toplayer = Bonsai_web_ui_toplayer

module Popover = struct
  let name = "Popover"

  let description =
    {|Most popovers are displayed relative to some "anchor" element.
      You can attach an `on_click` attribute to the anchor for opening the popover. |}
  ;;

  let view (local_ graph) =
    let computation, demo =
      [%demo
        let popover_anchor, { Toplayer.Controls.open_; _ } =
          Toplayer.Popover.create
            ~position:(return Toplayer.Position.Top)
            ~alignment:(return Toplayer.Alignment.Start)
            ~overflow_auto_wrapper:(Bonsai.return false)
            ~content:(fun ~close (local_ graph) ->
              let%arr theme = View.Theme.current graph
              and close in
              View.vbox
                [ View.text "Hi, I am a popover"
                ; View.button theme ~on_click:close "Close Popover"
                ])
            graph
        in
        let%arr theme = View.Theme.current graph
        and popover_anchor
        and open_ in
        View.button
          theme
          ~intent:Info
          ~attrs:[ popover_anchor; Vdom.Attr.on_click (fun _ -> open_) ]
          ~on_click:Effect.Ignore
          "Open Popover"]
    in
    let%arr computation in
    computation, demo
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Nested_popover = struct
  let name = "Nested popovers"
  let description = {|You can put popovers into other popovers! |}

  let view (local_ graph) =
    let computation, demo =
      [%demo
        let popover, { Toplayer.Controls.open_; _ } =
          Toplayer.Popover.create
            ~overflow_auto_wrapper:(Bonsai.return false)
            ~content:(fun ~close:_ (local_ graph) ->
              let button_with_popover (local_ graph) =
                let popover, { Toplayer.Controls.open_; _ } =
                  Toplayer.Popover.create
                    ~position:(return Toplayer.Position.Right)
                    ~alignment:(return Toplayer.Alignment.Start)
                    ~overflow_auto_wrapper:(Bonsai.return false)
                    ~content:(fun ~close:_ (local_ _graph) ->
                      View.text "I am a nested popover" |> Bonsai.return)
                    graph
                in
                let%arr popover
                and open_
                and theme = View.Theme.current graph in
                View.button
                  ~attrs:[ popover ]
                  theme
                  ~intent:Info
                  ~on_click:open_
                  "Open Inner Popover"
              in
              let%arr inner_button_1 = button_with_popover graph
              and inner_button_2 = button_with_popover graph in
              View.vbox [ View.text "Hi, I am a popover"; inner_button_1; inner_button_2 ])
            graph
        in
        let%arr theme = View.Theme.current graph
        and popover
        and open_ in
        View.button
          theme
          ~intent:Info
          ~attrs:[ popover; Vdom.Attr.on_click (fun _ -> open_) ]
          ~on_click:Effect.Ignore
          "Open Popover"]
    in
    let%arr computation in
    computation, demo
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Popover_with_arrow = struct
  let name = "Popover arrows"
  let description = {|Popovers can have arrows pointing to the floating element! |}

  let view (local_ graph) =
    let computation, demo =
      [%demo
        let popover_anchor, { Toplayer.Controls.open_; _ } =
          Toplayer.Popover.create
            ~overflow_auto_wrapper:(Bonsai.return false)
            ~has_arrow:(return true)
            ~content:(fun ~close:_ (local_ _graph) ->
              View.text "Hi, I am a popover" |> Bonsai.return)
            graph
        in
        let%arr theme = View.Theme.current graph
        and popover_anchor
        and open_ in
        View.button
          theme
          ~intent:Info
          ~attrs:[ popover_anchor; Vdom.Attr.on_click (fun _ -> open_) ]
          ~on_click:Effect.Ignore
          "Open Popover"]
    in
    let%arr computation in
    computation, demo
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Popover_close_on_click = struct
  let name = "Popovers close on click outside"

  let description =
    {| You can customize whether popovers close when clicked outside.
  By default, they close on clicks and escape, but not on right clicks. |}
  ;;

  let view (local_ graph) =
    let computation, demo =
      [%demo
        let offset = { Toplayer.Offset.main_axis = 8.; cross_axis = 0. } in
        let popover1_anchor, { Toplayer.Controls.open_ = open_1; _ } =
          Toplayer.Popover.create
            ~position:(return Toplayer.Position.Top)
            ~offset:(return offset)
            ~close_on_click_outside:(return Toplayer.Close_on_click_outside.No)
            ~overflow_auto_wrapper:(Bonsai.return false)
            ~content:(fun ~close:_ (local_ _graph) ->
              View.text "Will not close when clicked outside (use esc)" |> Bonsai.return)
            graph
        in
        let popover2_anchor, { Toplayer.Controls.open_ = open_2; _ } =
          Toplayer.Popover.create
            ~close_on_click_outside:
              (return Toplayer.Close_on_click_outside.Yes_unless_target_is_popover)
            ~position:(return Toplayer.Position.Right)
            ~offset:(return offset)
            ~overflow_auto_wrapper:(Bonsai.return false)
            ~content:(fun ~close:_ (local_ _graph) ->
              View.text
                "Will close when clicked outside, unless clicking on another popover"
              |> Bonsai.return)
            graph
        in
        let popover3_anchor, { Toplayer.Controls.open_ = open_3; _ } =
          Toplayer.Popover.create
            ~position:(return Toplayer.Position.Bottom)
            ~offset:(return offset)
            ~overflow_auto_wrapper:(Bonsai.return false)
            ~content:(fun ~close:_ (local_ _graph) ->
              View.text "Will always close when clicked outside" |> Bonsai.return)
            graph
        in
        let%arr theme = View.Theme.current graph
        and popover1_anchor
        and popover2_anchor
        and popover3_anchor
        and open_1
        and open_2
        and open_3 in
        View.button
          theme
          ~intent:Info
          ~attrs:
            [ popover1_anchor
            ; popover2_anchor
            ; popover3_anchor
            ; Vdom.Attr.on_click (fun _ -> Effect.Many [ open_1; open_2; open_3 ])
            ]
          ~on_click:Effect.Ignore
          "Open Popovers"]
    in
    let%arr computation in
    computation, demo
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end
