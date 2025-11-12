open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

let component (graph @ local) =
  let custom_title =
    Bonsai.return {%html|<div style="background: green">some custom heading here!</div>|}
  in
  let%demo_md docs =
    [ `Custom_heading (custom_title, 1, "Custom heading")
    ; `Markdown
        {|

    Welcome to the Bonsai Garden. This is a collection of views, components,
    computations, widgets, and gizmos that are useful for building applications using
    Bonsai.


    ## Links
    Like any garden, this one requires constant work and diligence to keep healthy.
    Please check back often to see changes and don't hesitate to reach out to your
    local Web Platform gardeners via email or at [#discuss-webdev](https://janestreet.slack.com/archives/C042XTQ7MH7). 
    |}
    ; Bonsai_garden_docs_common.custom_heading
        ~title:"Custom subheading"
        ~level:2
        (Bonsai.return {%html|<h3>subheading</h3>|})
    ]
  in
  Bonsai_garden_markdown_render_engine.generate_docs docs graph
;;
