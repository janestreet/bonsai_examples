open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Markdown = Bonsai_garden_markdown_render_engine
module Codemirror = Bonsai_web_ui_codemirror_read_only

let component (graph @ local) =
  let markdown_with_toc =
    [ `Markdown
        {markdown|

Extra text to move the page down

Extra text to move the page down

* # Heading1 title in bulleted list
* ## Heading2 title in bulleted list

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

# Intermediate title

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

# title should be green too [anchor testing here](#anchor_bottom){id=anchor_tag}
## title should be green too [anchor testing here](#anchor_bottom){id=anchor_tag}
### title should be green too [anchor testing here](#anchor_bottom){id=anchor_tag}
#### title should be green too [anchor testing here](#anchor_bottom){id=anchor_tag}
##### title should be green too [anchor testing here](#anchor_bottom){id=anchor_tag}
###### title should be green too [anchor testing here](#anchor_bottom){id=anchor_tag}

Extra text to move the page down

```ocaml

let () = 
    let foo = 3 in
    ()
```

* 1
* 2
* 3
* 4 
  * A
  * B
  * C

1. A
2. B
3. C
    ```ocaml
      (* I'm a child of C in list *)
    ```
4. D

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

# _Italicised_ __bold__ regular text **also bold**

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down


# `` `Polyvar of Code ``
Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down



A

50. Start at 50
51. Next
300. This number was set to 300 but will show up as 52




[Anchor tag that is far down](#anchor_tag){id=anchor_bottom}

    |markdown}
    ; `Component
        (Bonsai.return
           {%html|<div style="background-color: red">Render arbitrary vdom node on its own</div>|})
    ; Bonsai_garden_docs_common.singleton_doc
        (Static
           [%demo {%html|<div>Render arbitrary vdom node inside of example block</div>|}])
        graph
    ]
  in
  Markdown.generate_docs
    ~max_level:3
    ~code_block:(fun ~attributes:_ ~language code ->
      let language =
        match language with
        | "ocaml" -> Codemirror.Language.OCaml
        | _ -> Codemirror.Language.Plaintext
      in
      Codemirror.make ~language ~theme:Codemirror.Theme.Basic_light code)
    ~text:(fun ~attributes:_ text ->
      {%html|<span style="color: green; font-weight: bold">#{text}</span>|})
    markdown_with_toc
    graph
;;

let () = Bonsai_web.Start.start component ~enable_bonsai_telemetry:Enabled
