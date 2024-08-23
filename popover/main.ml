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
            ~content:(fun ~close:_ (local_ graph) ->
              let button_with_popover (local_ graph) =
                let popover, { Toplayer.Controls.open_; _ } =
                  Toplayer.Popover.create
                    ~position:(return Toplayer.Position.Right)
                    ~alignment:(return Toplayer.Alignment.Start)
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

module Old_popover = struct
  let name = "Popover"

  let description =
    {| This popover gives a warning that prompts for extra confirmation. |}
  ;;

  let view (local_ graph) =
    let vdom, demo =
      [%demo
        let theme = View.Theme.current graph in
        let popover_content ~close (local_ _graph) =
          let%arr close and theme in
          View.button theme ~on_click:close "Close popover"
        in
        let popover =
          Bonsai_web_ui_popover.component
            ~close_when_clicked_outside:(Bonsai.return true)
            ~direction:(Bonsai.return Bonsai_web_ui_popover.Direction.Right)
            ~alignment:(Bonsai.return Bonsai_web_ui_popover.Alignment.Center)
            ~popover:popover_content
            ()
            graph
        in
        let%arr { wrap; open_; close = _; toggle = _; is_open = _ } = popover
        and theme in
        wrap (View.button theme ~intent:Info ~on_click:open_ "Open Popover")]
    in
    Bonsai.map vdom ~f:(fun vdom -> vdom, demo)
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Old_context_menu_popover = struct
  let name = "Context menu popover"

  let description =
    {| This popover shows available actions, similar to a context menu. It also showcases nested popovers.|}
  ;;

  let view (local_ graph) =
    let computation, demo =
      [%demo
        let theme = View.Theme.current graph in
        let popover =
          Bonsai_web_ui_popover.component
            ~close_when_clicked_outside:(Bonsai.return true)
            ~direction:(Bonsai.return Bonsai_web_ui_popover.Direction.Right)
            ~alignment:(Bonsai.return Bonsai_web_ui_popover.Alignment.Center)
            ~popover:(fun ~close:_ (local_ _graph) ->
              let%arr theme in
              View.vbox
                [ View.text "Context Menu"
                ; View.button theme ~intent:Success ~on_click:Effect.Ignore "Action 1"
                ; View.button theme ~intent:Warning ~on_click:Effect.Ignore "Action 2"
                ])
            ()
            graph
        in
        let%arr { wrap; open_; _ } = popover in
        let attr =
          Vdom.Attr.on_contextmenu (fun _ ->
            Effect.Many [ open_; Effect.Prevent_default ])
        in
        wrap (View.text ~attrs:[ attr ] "Right click me!")]
    in
    Bonsai.map computation ~f:(fun vdom -> vdom, demo)
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
         , {| Popovers are toplayer elements that do not block interaction with the rest of
          the page. |}
         , [ Gallery.make_demo (module Popover)
           ; Gallery.make_demo (module Nested_popover)
           ; Gallery.make_demo (module Popover_with_arrow)
           ; Gallery.make_demo (module Popover_close_on_click)
           ] )
       ; ( "Bonsai_web_ui_popover"
         , {| Popovers are like tooltips, but they give you finer grain control as to when
         it opens and closes rather than just opening while hovering.
         `Bonsai_web_ui_popover` is an older implementation of popovers; new apps should
         use the toplayer version instead. |}
         , [ Gallery.make_demo (module Old_popover)
           ; Gallery.make_demo (module Old_context_menu_popover)
           ] )
       ])
    graph
;;

let () =
  Async_js.init ();
  Bonsai_web.Start.start component
;;
