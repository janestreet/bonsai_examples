open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Gallery = Bonsai_web_ui_gallery
module Toplayer = Bonsai_web_ui_toplayer

module Tooltip = struct
  let name = "Basic Tooltip"

  let description =
    {| The standard tooltip function allows annotating some element with a tooltip containing some text |}
  ;;

  let view (local_ _graph) =
    let%map () = return () in
    [%demo
      let non_interactive = Toplayer.Tooltip.text "I am a tooltip!" in
      let interactive =
        Toplayer.Tooltip.text ~hoverable_inside:true "I am an interactive tooltip!"
      in
      {%html|
        <%{View.hbox ~gap:(`Em 1)}>
          <span %{non_interactive}> Non-interactive Tooltip </span>
          <span %{interactive}> Interactive Tooltip </span>
        </>
      |}]
  ;;

  let selector = None
  let filter_attrs = None
end

module Tooltip_positioning = struct
  let name = "Tooltip positioning"

  let description =
    {| Tooltips can be explicitly positioned to the top, bottom, left, or
  right of the element. If no position is specified, they will be automatically placed on
  the side with the most space.  |}
  ;;

  let view (local_ _graph) =
    let%map () = return () in
    let vbox = View.vbox ~gap:(`Em 1) in
    let hbox = View.hbox ~gap:(`Em 1) ~main_axis_alignment:Space_between in
    [%demo
      let tooltip position alignment =
        Toplayer.Tooltip.text ~position ~alignment "Hi, I am a tooltip"
      in
      {%html|
        <%{vbox}>
          <%{hbox}>
            <span %{tooltip Top Start}> Top, start </span>
            <span %{tooltip Top Center}> Top, center </span>
            <span %{tooltip Top End}> Top, end </span>
          </>
          <%{hbox}>
            <span %{tooltip Bottom Start}> Bottom, start </span>
            <span %{tooltip Bottom Center}> Bottom, center </span>
            <span %{tooltip Bottom End}> Bottom, end </span>
          </>
          <%{hbox}>
            <span %{tooltip Left Start}> Left, start </span>
            <span %{tooltip Left Center}> Left, center </span>
            <span %{tooltip Left End}> Left, end </span>
          </>
          <%{hbox}>
            <span %{tooltip Right Start}> Right, start </span>
            <span %{tooltip Right Center}> Right, center </span>
            <span %{tooltip Right End}> Right, end </span>
          </>
        </>
      |}]
  ;;

  let selector = None
  let filter_attrs = None
end

let rickroll =
  Effect.open_url ~in_:New_tab_or_window "https://www.youtube.com/watch?v=dQw4w9WgXcQ"
;;

module Tooltip_with_arbitrary_content = struct
  let name = "Tooltips with arbitrary content"
  let description = {| With `Tooltip.create`, tooltips can contain any `Vdom.Node.t` |}

  let view (local_ _graph) =
    let%map () = return () in
    [%demo
      let tooltip =
        Toplayer.Tooltip.create
          ~hoverable_inside:true
          {%html|
            <%{View.vbox}>
              do not click this button
              <button on_click=%{fun _ -> rickroll}>no clicky!</button>
            </>
          |}
      in
      {%html|<span %{tooltip}>cursed knowledge</span>|}]
  ;;

  let selector = None
  let filter_attrs = None
end
