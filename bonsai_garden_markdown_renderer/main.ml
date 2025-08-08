open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Markdown = Bonsai_garden_markdown_render_engine
module Codemirror = Bonsai_web_ui_codemirror_read_only

let markdown_with_toc =
  [ `Markdown
      {markdown|

Extra text to move the page down

Extra text to move the page down

* # huh
* ## huhhh
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
# hi


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

some  kinda weird text
```ocaml

let () = 
    let test_value = 3 in
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

# _Hmm_ __what__ **yikes**
Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down

Extra text to move the page down


# `` `Interesting of Code ``
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
300. This might show up as something else since it's part of the previous list




[very far down](#anchor_tag){id=anchor_bottom}

    |markdown}
  ; `Component (Bonsai.return {%html|<div>Testing component rendering</div>|})
  ; `Component_example
      ( Bonsai.return {%html|<div>Testing component EXAMPLE rendering</div>|}
      , Bonsai_component_documentation_codeblock.Language.OCaml
      , Bonsai_component_documentation_codeblock.Language.Map.of_alist_exn
          [ ( Bonsai_component_documentation_codeblock.Language.OCaml
            , {|
                          let () = () in
                          `Hi
               |}
            )
          ] )
  ]
;;

let component graph =
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
