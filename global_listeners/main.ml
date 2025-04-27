open! Core
open! Bonsai_web

let component (local_ _graph) =
  Vdom.Node.div
    [ Vdom.Node.div
        ~attrs:
          [ Vdom.Attr.Global_listeners.click ~phase:Capture ~f:(fun _ ->
              Effect.print_s [%message "global listener on capture"])
          ; Vdom.Attr.Global_listeners.click ~phase:Bubbling ~f:(fun _ ->
              Effect.print_s [%message "global listener on bubbling"])
          ; Vdom.Attr.on_click (fun _ -> Effect.print_s [%message "listener on parent"])
          ]
        []
    ; Vdom.Node.div
        [ Vdom.Node.button
            ~attrs:
              [ Vdom.Attr.on_click (fun _ ->
                  Effect.print_s [%message "listener on clicked button"])
              ]
            [ View.text "No stop propagation" ]
        ; Vdom.Node.button
            ~attrs:
              [ Vdom.Attr.on_click (fun _ ->
                  Effect.Many
                    [ Effect.Stop_propagation
                    ; Effect.print_s
                        [%message "listener on clicked button, stop propagation"]
                    ])
              ]
            [ View.text "With Stop propagation" ]
        ]
    ]
  |> Bonsai.return
;;

let () = Bonsai_web.Start.start component ~enable_bonsai_telemetry:Enabled
