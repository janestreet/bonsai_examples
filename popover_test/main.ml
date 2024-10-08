open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Gallery = Bonsai_web_ui_gallery

module Two_left_click_popovers = struct
  let name = "Multiple context menu popovers"

  let description =
    {|This test tests against a regression where clicking on the outside to click a context menu, also opened chrome's context menu.|}
  ;;

  let view graph =
    let vdom, demo =
      [%demo
        let theme = View.Theme.current graph in
        let popover_content ~close:_ _graph =
          Bonsai.return (View.text "Popover contents")
        in
        let popover graph =
          let popover =
            (Bonsai_web_ui_popover_deprecated.component [@alert "-deprecated"])
              ~close_when_clicked_outside:(Bonsai.return true)
              ~allow_event_propagation_when_clicked_outside:
                (Bonsai.return (function
                  | `Left_click | `Escape -> false
                  | `Right_click -> true))
              ~direction:(Bonsai.return Bonsai_web_ui_popover_deprecated.Direction.Right)
              ~alignment:(Bonsai.return Bonsai_web_ui_popover_deprecated.Alignment.Center)
              ~popover:popover_content
              ()
              graph
          in
          let%arr { wrap; open_; close = _; toggle = _; is_open = _ } = popover
          and theme in
          wrap
            (View.button
               theme
               ~intent:Info
               ~attrs:
                 [ Vdom.Attr.on_contextmenu (fun _ ->
                     Effect.Many [ open_; Effect.Prevent_default ])
                 ]
               ~on_click:open_
               "toggle popover")
        in
        let%map p1 = popover graph
        and p2 = popover graph in
        Vdom.Node.div [ p1; p2 ]]
    in
    Bonsai.map vdom ~f:(fun vdom -> vdom, demo)
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

let component graph =
  let%sub theme, theme_picker = Gallery.Theme_picker.component () graph in
  View.Theme.set_for_app
    theme
    (Gallery.make_sections
       ~theme_picker
       [ ( "Popover tests"
         , {|This file cont|}
         , [ Gallery.make_demo (module Two_left_click_popovers) ] )
       ])
    graph
;;

let () =
  Async_js.init ();
  Bonsai_web.Start.start component
;;
