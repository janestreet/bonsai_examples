open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Gallery = Bonsai_web_ui_gallery
module Toplayer = Bonsai_web_ui_toplayer

module Vdom_popover = struct
  let name = "Popover"

  let description =
    {|A popover is positioned relative to a target element. `Vdom_toplayer`'s popovers
    are always displayed; for "sometimes open" popovers, use Bonsai-state-backed
    `Bonsai_web_ui_toplayer` popovers.

    We can use the `~position` and `~alignment` attributes to control where the tooltip
    is placed. By default, `Auto` and `Center` are used, which will place the tooltip on the side
    with the most space available. |}
  ;;

  let view (local_ _graph) =
    let vdom, demo =
      [%demo
        let popover position alignment =
          Vdom_toplayer.popover
            ~position
            ~alignment
            ~offset:{ main_axis = 6.; cross_axis = 0. }
            (Vdom.Node.div [ View.text "Hi, I am a popover" ])
        in
        Vdom.Node.div
          ~attrs:[ popover Top Center; popover Left End ]
          [ View.text "oooo, popovers!" ]]
    in
    return (vdom, demo)
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Vdom_popover_virtual = struct
  let name = "Virtually-positioned Popovers"

  let description =
    {|You can also position popovers relative to a "virtual" anchor, which is typically
    a bounding box or coordinate. |}
  ;;

  let view (local_ graph) =
    let vdom, demo =
      [%demo
        let coords, set_coords = Bonsai.state_opt graph in
        let contents =
          match%sub coords with
          | None -> return [ Vdom.Node.text "Click to place!" ]
          | Some (x, y) ->
            let%arr x and y and set_coords in
            let anchor = Floating_positioning_new.Anchor.of_coordinate ~x ~y in
            [ Vdom_toplayer.For_use_in_portals.popover_custom
                ~position:Right
                ~alignment:Start
                ~popover_content:(View.text "Click around to move me!")
                anchor
            ; Vdom.Node.button
                ~attrs:
                  [ Vdom.Attr.on_click (fun _ ->
                      Effect.Many [ set_coords None; Effect.Stop_propagation ])
                  ]
                [ View.text "remove popover" ]
            ]
        in
        let%arr contents and set_coords in
        Vdom.Node.div
          ~attrs:
            [ [%css
                {|
                  width: 100%;
                  height: 300px;
                |}]
            ; Vdom.Attr.on_click (fun evt ->
                set_coords
                  (Some (evt##.clientX |> Int.to_float, evt##.clientY |> Int.to_float)))
            ]
          contents]
    in
    let%arr vdom in
    vdom, demo
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Vdom_popover_match_anchor_side_length = struct
  let name = "Dropdown-like Popovers"

  let description =
    {|When building dropdowns, you might want your popover to have the same width/height
    as the side of the anchor to which it attached. You can also constrain the min or max
    width/height.

    This can be done by seting `match_anchor_side_length`.
    Resize the anchors below to see this in action! |}
  ;;

  let view (local_ _graph) =
    let vdom, demo =
      [%demo
        let resizable =
          [%css
            {|
              resize: both;
              overflow: hidden;
              padding: 10px;
              border: 1px solid black;
            |}]
        in
        let popover ~match_anchor_side_length position =
          Vdom_toplayer.popover
            ~popover_attrs:[ [%css {|overflow: hidden;|}] ]
            ~position
            ~match_anchor_side_length
            (View.text "Dropdown")
        in
        View.vbox
          ~attrs:[ [%css {|width: 100%;|}] ]
          ~gap:(`Px 60)
          [ View.hbox
              ~attrs:[ [%css {|width: 100%;|}] ]
              ~main_axis_alignment:Space_around
              [ Vdom.Node.div
                  ~attrs:
                    [ resizable
                    ; popover ~match_anchor_side_length:Shrink_to_match Bottom
                    ]
                  [ View.text "Popover attached below: shrink" ]
              ; Vdom.Node.div
                  ~attrs:
                    [ resizable; popover ~match_anchor_side_length:Shrink_to_match Right ]
                  [ View.text "Popover attached to the right: shrink" ]
              ]
          ; View.hbox
              ~attrs:[ [%css {|width: 100%;|}] ]
              ~main_axis_alignment:Space_around
              [ Vdom.Node.div
                  ~attrs:
                    [ resizable; popover ~match_anchor_side_length:Match_exactly Bottom ]
                  [ View.text "Popover attached below: exact" ]
              ; Vdom.Node.div
                  ~attrs:
                    [ resizable; popover ~match_anchor_side_length:Match_exactly Right ]
                  [ View.text "Popover attached to the right: exact" ]
              ]
          ; View.hbox
              ~attrs:[ [%css {|width: 100%;|}] ]
              ~main_axis_alignment:Space_around
              [ Vdom.Node.div
                  ~attrs:
                    [ resizable; popover ~match_anchor_side_length:Grow_to_match Bottom ]
                  [ View.text "Popover attached below: grow" ]
              ; Vdom.Node.div
                  ~attrs:
                    [ resizable; popover ~match_anchor_side_length:Grow_to_match Right ]
                  [ View.text "Popover attached to the right: grow" ]
              ]
          ]]
    in
    return (vdom, demo)
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Vdom_popover_arrows = struct
  let name = "Popover arrows"

  let description =
    {| The optional `arrow` argument allows us to place a "pointer" element from the popover
    to the anchor. The arrow will be rotated so that the top of the element you provide
    points towards the anchor. |}
  ;;

  let view (local_ _graph) =
    let vdom, demo =
      [%demo
        let arrow =
          let side_len = `Px_float 8. in
          Vdom.Node.div
            ~attrs:
              [ [%css
                  {|
                    height: %{side_len#Css_gen.Length};
                    width: %{side_len#Css_gen.Length};
                    transform: rotate(45deg);
                    background-color: Canvas;
                    border: solid;
                    border-bottom: none;
                    border-right: none;
                    top: -1.5px;
                    position: relative;
                  |}]
              ]
            []
        in
        let popover position alignment =
          Vdom_toplayer.popover
            ~position
            ~alignment
            ~offset:{ main_axis = 10.; cross_axis = 0. }
            ~arrow
            (Vdom.Node.div [ View.text "Hi, I am a popover" ])
        in
        Vdom.Node.div
          ~attrs:[ popover Top Center; popover Left End ]
          [ View.text "oooo, popovers!" ]]
    in
    return (vdom, demo)
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Vdom_tooltip = struct
  let name = "Tooltip"

  let description =
    {|Tooltips are hover-triggered popovers. Only one tooltip may be open at a time,
    unless they are nested. Also, unlike popovers, only one tooltip may be attached to
    any given element.
    |}
  ;;

  let view (local_ _graph) =
    [%demo
      let tooltip position alignment =
        Vdom_toplayer.tooltip
          ~position
          ~alignment
          ~hide_grace_period:(Time_ns.Span.of_int_ms 300)
          ~hoverable_inside:true
          (View.text "Hi, I am a tooltip")
      in
      View.vbox
        ~gap:(`Px 10)
        [ View.hbox
            ~main_axis_alignment:Space_between
            ~gap:(`Px 10)
            [ Vdom.Node.div ~attrs:[ tooltip Top Start ] [ View.text "Top, start" ]
            ; Vdom.Node.div ~attrs:[ tooltip Top Center ] [ View.text "Top, center" ]
            ; Vdom.Node.div ~attrs:[ tooltip Top End ] [ View.text "Top, end" ]
            ]
        ; View.hbox
            ~main_axis_alignment:Space_between
            ~gap:(`Px 10)
            [ Vdom.Node.div ~attrs:[ tooltip Bottom Start ] [ View.text "Bottom, start" ]
            ; Vdom.Node.div
                ~attrs:[ tooltip Bottom Center ]
                [ View.text "Bottom, center" ]
            ; Vdom.Node.div ~attrs:[ tooltip Bottom End ] [ View.text "Bottom, end" ]
            ]
        ; View.hbox
            ~main_axis_alignment:Space_between
            ~gap:(`Px 10)
            [ Vdom.Node.div ~attrs:[ tooltip Left Start ] [ View.text "Left, start" ]
            ; Vdom.Node.div ~attrs:[ tooltip Left Center ] [ View.text "Left, center" ]
            ; Vdom.Node.div ~attrs:[ tooltip Left End ] [ View.text "Left, end" ]
            ]
        ; View.hbox
            ~main_axis_alignment:Space_between
            ~gap:(`Px 10)
            [ Vdom.Node.div ~attrs:[ tooltip Right Start ] [ View.text "Right, start" ]
            ; Vdom.Node.div ~attrs:[ tooltip Right Center ] [ View.text "Right, center" ]
            ; Vdom.Node.div ~attrs:[ tooltip Right End ] [ View.text "Right, end" ]
            ]
        ]]
    |> return
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Vdom_tooltip_hoverable = struct
  let name = "Hoverable Tooltips"

  let description =
    {|The `hoverable_inside` argument controls whether tooltips will stay around if
      hovered inside. You'll need to also provide a `hide_grace_period`, so the user has
      enough time to move their mouse into the tooltip. |}
  ;;

  let view (local_ _graph) =
    [%demo
      let tooltip ~hoverable_inside =
        Vdom_toplayer.tooltip
          ~show_delay:(Time_ns.Span.of_int_ms 100)
          ~hide_grace_period:(Time_ns.Span.of_int_ms 100)
          ~hoverable_inside
          (View.text "Hi, I am a tooltip")
      in
      View.hbox
        ~gap:(`Px 10)
        [ View.vbox
            ~attrs:[ tooltip ~hoverable_inside:false ]
            [ View.text "Show on hover" ]
        ; View.vbox
            ~attrs:[ tooltip ~hoverable_inside:true ]
            [ View.text "Show on hover (interactive)" ]
        ]]
    |> return
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Vdom_tooltip_nested = struct
  let name = "Nested tooltips"
  let description = {| Demonstration of nested tooltips, controlled via hover.|}

  let view _ =
    [%demo
      let make_tooltip content =
        Vdom_toplayer.tooltip
          ~tooltip_attrs:
            [ [%css
                {|
                  padding: 0;
                  border: none;
                  box-sizing: border-box;
                |}]
            ]
          ~position:Right
          ~alignment:Start
          ~offset:{ main_axis = 2.; cross_axis = 0. }
          ~show_delay:(Time_ns.Span.of_int_ms 100)
          ~hide_grace_period:(Time_ns.Span.of_int_ms 100)
          ~hoverable_inside:true
          (Vdom.Node.div content)
      in
      let menu_element ?(tooltip_content = []) content =
        let tooltip =
          match tooltip_content with
          | [] -> Vdom.Attr.empty
          | content -> make_tooltip content
        in
        View.hbox
          ~attrs:
            [ [%css
                {|
                  padding: 4px;
                  border: 1px solid black;
                  box-sizing: border-box;
                |}]
            ; tooltip
            ]
          [ content ]
      in
      menu_element
        (View.text "Hover for menu")
        ~tooltip_content:
          [ View.vbox
              [ menu_element
                  (View.text "Hover for Submenu 1")
                  ~tooltip_content:
                    [ menu_element
                        (View.text "Nested thrice!")
                        ~tooltip_content:[ menu_element (View.text "hi") ]
                    ]
              ; menu_element
                  (View.text "Hover for Submenu 2")
                  ~tooltip_content:
                    [ menu_element
                        (View.text
                           "It would be nice to demonstrate how this all would look if \
                            there was quite a lot of text. I'm not sure what the best \
                            way to do this is, but maybe eventually something will come \
                            to mind? ")
                        ~tooltip_content:
                          [ menu_element
                              (View.text "level 4")
                              ~tooltip_content:
                                [ menu_element (View.text "Ok, that's enough") ]
                          ]
                    ]
              ]
          ]]
    |> Bonsai.return
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Nested_popovers_remain_open = struct
  let name = "Nested popovers"
  let description = {| Child popovers remain open when their parents close and re-open. |}

  let view (local_ graph) =
    let computation, demo =
      [%demo
        let popover, { Toplayer.Controls.open_; _ } =
          Toplayer.Popover.create
            ~close_on_click_outside:(Bonsai.return Toplayer.Close_on_click_outside.No)
            ~content:(fun ~close (local_ graph) ->
              let button_with_popover (local_ graph) =
                let popover, { Toplayer.Controls.open_; _ } =
                  Toplayer.Popover.create
                    ~close_on_click_outside:
                      (Bonsai.return Toplayer.Close_on_click_outside.No)
                    ~position:(return Toplayer.Position.Right)
                    ~alignment:(return Toplayer.Alignment.Start)
                    ~content:(fun ~close (local_ graph) ->
                      let%arr theme = View.Theme.current graph
                      and close in
                      View.vbox
                        [ View.text "I am a nested popover"
                        ; View.button theme ~intent:Error ~on_click:close "Close"
                        ])
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
              let%arr theme = View.Theme.current graph
              and inner_button_1 = button_with_popover graph
              and inner_button_2 = button_with_popover graph
              and close in
              View.vbox
                [ View.text "Hi, I am a popover"
                ; inner_button_1
                ; inner_button_2
                ; View.button theme ~intent:Error ~on_click:close "Close"
                ])
            graph
        in
        let%arr theme = View.Theme.current graph
        and popover
        and open_ in
        View.button theme ~intent:Info ~attrs:[ popover ] ~on_click:open_ "Open Popover"]
    in
    let%arr computation in
    computation, demo
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Theme_arrows = struct
  let name = "Tooltips with arrows"

  let description =
    {| Themes can toggle whether tooltips have arrows through theme `Constants.t`. |}
  ;;

  let view (local_ graph) =
    let%map theme = View.Theme.current graph in
    [%demo
      let tooltip =
        View.tooltip_attr theme ~hoverable_inside:true "this tooltip has an arrow"
      in
      View.text ~attrs:[ tooltip ] "hover me!"]
  ;;

  let view =
    View.Theme.override_constants_for_computation
      ~f:(fun constants ->
        { constants with
          toplayer =
            { constants.toplayer with
              tooltips_have_arrows = `Yes_with_length_px 8.
            ; tooltip_offset_px = 12.
            }
        })
      view
  ;;

  let selector = None
  let filter_attrs = None
end

module Vdom_tooltip_animations = struct
  let name = "Animating Tooltips"

  let description =
    {|It's possible to animate the showing of tooltips.
      This is a bit janky though, and should be considered highly experimental.
      Animating hiding does not currently work, because the tooltip DOM is removed when
      tooltips are closed.
       |}
  ;;

  let view (local_ _graph) =
    [%demo
      let module Style =
        [%css
        stylesheet
          {|
            @keyframes fade-in {
              0% {
                opacity: 0;
                transform: scaleX(0);
              }

              100% {
                opacity: 1;
                transform: scaleX(1);
              }
            }

            @keyframes fade-out-broken {
              0% {
                opacity: 1;
                transform: scaleX(1);
                /* display needed on the closing animation to keep the popover
                visible until the animation ends */
                display: flex;
              }

              100% {
                opacity: 0;
                transform: scaleX(0);
              }
            }

            .animated_tooltip[popover]:not(:popover-open) {
              /* The `color: red` helps verify that the animation itself isn't the
                 problem, but that the popover never ends up in this state. */
              color: red;
              animation: fade-out-broken 0.7s ease-out;
            }

            .animated_tooltip[popover]:popover-open {
              animation: fade-in 0.7s ease-out;
            }
            |}]
      in
      let tooltip =
        Vdom_toplayer.tooltip
          ~tooltip_attrs:[ Style.animated_tooltip ]
          ~show_delay:(Time_ns.Span.of_int_ms 100)
          ~hide_grace_period:(Time_ns.Span.of_int_ms 100)
          (Vdom.Node.div [ View.text "Hi, I am a tooltip" ])
      in
      View.hbox
        ~gap:(`Px 10)
        [ View.vbox ~attrs:[ tooltip ] [ View.text "Show on hover" ] ]]
    |> return
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Giant_toplayer_elements = struct
  let name = "Giant toplayer elements"
  let description = {|Tests for oversized popovers and modals|}

  let view (local_ graph) =
    let computation, demo =
      [%demo
        let content =
          View.vbox
            (Vdom.Node.h1 [ View.text "Popover Heading" ]
             :: List.init 100 ~f:(fun _ -> View.text (String.make 1000 'z')))
        in
        let popover, { Toplayer.Controls.open_ = open_popover; _ } =
          Toplayer.Popover.create ~content:(fun ~close:_ _ -> return content) graph
        in
        let { Toplayer.Controls.open_ = open_virtual_popover; _ } =
          Toplayer.Popover.create_virtual
            ~content:(fun ~close:_ _ -> return content)
            (return (Toplayer.Anchor.of_coordinate ~x:10. ~y:10.))
            graph
        in
        let { Toplayer.Controls.open_ = open_virtual_aligned_popover; _ } =
          Toplayer.Popover.create_virtual
            ~content:(fun ~close:_ _ -> return content)
            ~alignment:(return Toplayer.Alignment.Start)
            (return (Toplayer.Anchor.of_coordinate ~x:10. ~y:40.))
            graph
        in
        let { Toplayer.Controls.open_ = open_modal; _ } =
          Toplayer.Modal.create ~content:(fun ~close:_ _ -> return content) graph
        in
        let%arr theme = View.Theme.current graph
        and popover
        and open_popover
        and open_virtual_popover
        and open_virtual_aligned_popover
        and open_modal in
        View.hbox
          [ View.text ~attrs:[ View.tooltip_attr' theme [ content ] ] "Tooltip"
          ; View.button ~attrs:[ popover ] theme ~on_click:open_popover "Open Popover"
          ; View.button theme ~on_click:open_virtual_popover "Open Virtual Popover"
          ; View.button
              theme
              ~on_click:open_virtual_aligned_popover
              "Open Virtual Aligned Popover"
          ; View.button theme ~on_click:open_modal "Open Modal"
          ]]
    in
    let%arr computation in
    computation, demo
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Autofocus_on_open = struct
  let name = "Autofocus on open"

  let description =
    {| This example tests that popover autofocus won't steal focus from something already
    focused inside the popover. `Effect.Focus.on_activate` doesn't currently work, because
    the popover becomes visible a frame or two after the computation is activated. |}
  ;;

  let view (local_ graph) =
    let computation, demo =
      [%demo
        let focus_style =
          [%css
            {|
              &:focus {
                border: 2px solid blue;
              }
            |}]
        in
        let popover_autofocus, { Toplayer.Controls.open_ = open_popover_autofocus; _ } =
          Toplayer.Popover.create
            ~focus_on_open:(return true)
            ~extra_attrs:(return [ focus_style ])
            ~content:(fun ~close:_ (local_ _graph) ->
              return (Vdom.Node.input ~attrs:[ Vdom.Attr.autofocus true ] ()))
            graph
        in
        let ( popover_focus_on_activate
            , { Toplayer.Controls.open_ = open_popover_focus_on_activate; _ } )
          =
          Toplayer.Popover.create
            ~focus_on_open:(return true)
            ~extra_attrs:(return [ focus_style ])
            ~content:(fun ~close:_ (local_ graph) ->
              let focus_attr = Effect.Focus.on_activate () graph in
              let%arr focus_attr in
              Vdom.Node.input ~attrs:[ focus_attr ] ())
            graph
        in
        let ( popover_no_autofocus
            , { Toplayer.Controls.open_ = open_popover_no_autofocus; _ } )
          =
          Toplayer.Popover.create
            ~focus_on_open:(return true)
            ~extra_attrs:(return [ focus_style ])
            ~content:(fun ~close:_ (local_ _graph) -> return (Vdom.Node.input ()))
            graph
        in
        let { Toplayer.Controls.open_ = open_modal_autofocus; _ } =
          Toplayer.Modal.create
            ~extra_attrs:(return [ focus_style ])
            ~content:(fun ~close:_ (local_ _graph) ->
              return (Vdom.Node.input ~attrs:[ Vdom.Attr.autofocus true ] ()))
            graph
        in
        let { Toplayer.Controls.open_ = open_modal_focus_on_activate; _ } =
          Toplayer.Modal.create
            ~extra_attrs:(return [ focus_style ])
            ~content:(fun ~close:_ (local_ graph) ->
              let focus_attr = Effect.Focus.on_activate () graph in
              let%arr focus_attr in
              Vdom.Node.input ~attrs:[ focus_attr ] ())
            graph
        in
        let { Toplayer.Controls.open_ = open_modal_no_autofocus; _ } =
          Toplayer.Modal.create
            ~extra_attrs:(return [ focus_style ])
            ~content:(fun ~close:_ (local_ _graph) -> return (Vdom.Node.input ()))
            graph
        in
        let%arr theme = View.Theme.current graph
        and popover_autofocus
        and popover_focus_on_activate
        and popover_no_autofocus
        and open_modal_autofocus
        and open_modal_focus_on_activate
        and open_modal_no_autofocus
        and open_popover_autofocus
        and open_popover_focus_on_activate
        and open_popover_no_autofocus in
        View.vbox
          ~gap:(`Px 4)
          [ View.button
              theme
              ~intent:Info
              ~attrs:[ popover_autofocus ]
              ~on_click:open_popover_autofocus
              "Popover with autofocus child"
          ; View.button
              theme
              ~intent:Info
              ~attrs:[ popover_focus_on_activate ]
              ~on_click:open_popover_focus_on_activate
              "Popover with Effect.Focus.on_activate child"
          ; View.button
              theme
              ~intent:Info
              ~attrs:[ popover_no_autofocus ]
              ~on_click:open_popover_no_autofocus
              "Popover without autofocus child"
          ; View.button
              theme
              ~intent:Info
              ~on_click:open_modal_autofocus
              "Modal with autofocus child"
          ; View.button
              theme
              ~intent:Info
              ~on_click:open_modal_focus_on_activate
              "Modal with Effect.Focus.on_activate child"
          ; View.button
              theme
              ~intent:Info
              ~on_click:open_modal_no_autofocus
              "Modal without autofocus child"
          ]]
    in
    let%arr computation in
    computation, demo
  ;;

  let selector = None
  let filter_attrs = None
end

let component (local_ graph) =
  let%sub theme, theme_picker = Gallery.Theme_picker.component () graph in
  let view =
    Gallery.make_sections
      ~theme_picker
      [ ( "Vdom Toplayer"
        , {| vdom_toplayer provides basic, vdom-only primitives used to implement
             `bonsai_web_ui_toplayer`. This library is intended for component library
             and theme authors. |}
        , [ Gallery.make_demo (module Vdom_popover)
          ; Gallery.make_demo (module Vdom_popover_virtual)
          ; Gallery.make_demo (module Vdom_popover_match_anchor_side_length)
          ; Gallery.make_demo (module Vdom_popover_arrows)
          ; Gallery.make_demo (module Vdom_tooltip)
          ; Gallery.make_demo (module Vdom_tooltip_hoverable)
          ; Gallery.make_demo (module Vdom_tooltip_nested)
          ] )
      ; ( "Interesting Behavior Demos"
        , {| These demos are intended for Bonsai developers. |}
        , [ Gallery.make_demo (module Nested_popovers_remain_open)
          ; Gallery.make_demo (module Theme_arrows)
          ; Gallery.make_demo (module Vdom_tooltip_animations)
          ; Gallery.make_demo (module Giant_toplayer_elements)
          ; Gallery.make_demo (module Autofocus_on_open)
          ] )
      ]
  in
  View.Theme.set_for_app theme view graph
;;

let () =
  Async_js.init ();
  Bonsai_web.Start.start component
;;
