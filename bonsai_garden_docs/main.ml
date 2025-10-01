open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Docs = Bonsai_garden_docs_lib

module _ =
  [%css
  stylesheet
    {|
      body {
        margin: 0;
      }
    |}]

let run () =
  (fun (graph @ local) ->
    Docs.(
      make
        ~not_found_page:(fun (_graph @ local) ->
          {%html|
            <div
              style="
                display: flex;
                flex-direction: column;
                align-items: center;
                width: 100vw;
              "
            >
              <div style="white-space: pre">
                <h1>
                  If running from Bonsai examples using fslink, make sure you're serving the
                  files directly from this directory
                </h1>
                <h2>Not Found</h2>
                <div>This page doesn't exist or was moved from another place.</div>
              </div>
            </div>
          |}
          |> Bonsai.return)
        Library.
          [ create
              ~index_page:Bonsai_garden_front_page.component
              ~as_index_route:true
              ~name:"Bonsai Garden"
              ~logo:(Bonsai.return (`Img_src "bonsai.svg"))
              Category.
                [ page_with_sidebar
                    ~name:"Page 1"
                    Sidebar.
                      [ page ~name:"Snips" (module Bonsai_garden_snips_docs)
                      ; section
                          ~name:"UI Component"
                          Section.
                            [ page
                                ~name:"Accordion"
                                (module Bonsai_garden_web_ui_accordion_docs)
                            ; page ~name:"Toplayer" (module Bonsai_garden_toplayer_docs)
                            ]
                      ; section
                          ~name:"Library"
                          Section.
                            [ page ~name:"Snips" (module Bonsai_garden_snips_docs)
                            ; subsection
                                ~name:"PPX"
                                Subsection.
                                  [ page
                                      ~name:"PPX HTML"
                                      (module Bonsai_garden_ppx_html_docs)
                                  ; page
                                      ~name:"PPX CSS"
                                      (module Bonsai_garden_ppx_css_docs)
                                  ]
                            ]
                      ]
                ; page_with_sidebar
                    ~name:"Page 2"
                    Sidebar.
                      [ page ~name:"Snips" (module Bonsai_garden_snips_docs)
                      ; section
                          ~name:"UI Component"
                          Section.
                            [ page
                                ~name:"Accordion"
                                (module Bonsai_garden_web_ui_accordion_docs)
                            ; page ~name:"Toplayer" (module Bonsai_garden_toplayer_docs)
                            ]
                      ; section
                          ~name:"Library"
                          Section.
                            [ page ~name:"Snips" (module Bonsai_garden_snips_docs)
                            ; subsection
                                ~name:"PPX"
                                Subsection.
                                  [ page
                                      ~name:"PPX HTML"
                                      (module Bonsai_garden_ppx_html_docs)
                                  ; page
                                      ~name:"PPX CSS"
                                      (module Bonsai_garden_ppx_css_docs)
                                  ]
                            ]
                      ]
                ; custom_page ~name:"Page for testing not found" (fun _graph ->
                    Bonsai.return
                      {%html|
                        <div>
                          <a href="/nonexistent-url"> Click this link to go to a nonexistent url </a>
                        </div>
                      |})
                ]
          ; create
              ~index_page:Bonsai_garden_front_page.component
              ~name:"Different one"
              ~logo:(Bonsai.return (`Img_src "bonsai.svg"))
              Category.
                [ page_with_sidebar
                    ~name:"Page 1"
                    Sidebar.
                      [ page ~name:"Snips" (module Bonsai_garden_snips_docs)
                      ; section
                          ~name:"UI Component"
                          Section.
                            [ page
                                ~name:"Accordion"
                                (module Bonsai_garden_web_ui_accordion_docs)
                            ; page ~name:"Toplayer" (module Bonsai_garden_toplayer_docs)
                            ]
                      ; section
                          ~name:"Library"
                          Section.
                            [ page ~name:"Snips" (module Bonsai_garden_snips_docs)
                            ; subsection
                                ~name:"PPX"
                                Subsection.
                                  [ page
                                      ~name:"PPX HTML"
                                      (module Bonsai_garden_ppx_html_docs)
                                  ; page
                                      ~name:"PPX CSS"
                                      (module Bonsai_garden_ppx_css_docs)
                                  ]
                            ]
                      ]
                ; page_with_sidebar
                    ~name:"Page 2"
                    Sidebar.
                      [ page ~name:"Snips" (module Bonsai_garden_snips_docs)
                      ; section
                          ~name:"UI Component"
                          Section.
                            [ page
                                ~name:"Accordion"
                                (module Bonsai_garden_web_ui_accordion_docs)
                            ; page ~name:"Toplayer" (module Bonsai_garden_toplayer_docs)
                            ]
                      ; section
                          ~name:"Library"
                          Section.
                            [ page ~name:"Snips" (module Bonsai_garden_snips_docs)
                            ; subsection
                                ~name:"PPX"
                                Subsection.
                                  [ page
                                      ~name:"PPX HTML"
                                      (module Bonsai_garden_ppx_html_docs)
                                  ; page
                                      ~name:"PPX CSS"
                                      (module Bonsai_garden_ppx_css_docs)
                                  ]
                            ]
                      ]
                ; custom_page ~name:"Page for testing not found" (fun _graph ->
                    Bonsai.return
                      {%html|
                        <div>
                          <a href="/nonexistent-url"> Click this link to go to a nonexistent url </a>
                        </div>
                      |})
                ]
          ])
      graph)
  |> Bonsai_web.Start.start ~enable_bonsai_telemetry:Enabled
;;

let () = run ()
