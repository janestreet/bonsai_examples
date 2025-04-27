open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Tabs = Bonsai_web_ui_tabs

module T = struct
  type t =
    | A
    | B
    | C
  [@@deriving sexp, equal, compare, enumerate]
end

let component (local_ graph) =
  let tab_state =
    Tabs.tab_state ~sexp_of:[%sexp_of: T.t] ~initial:T.A ~equal:[%equal: T.t] graph
  in
  let contents =
    Tabs.tab_ui
      ~equal:[%equal: T.t]
      ~sexp_of:[%sexp_of: T.t]
      tab_state
      ~all_tabs:(Bonsai.return T.all)
      ~f:(fun ~change_tab tab (local_ graph) ->
        Bonsai.enum
          (module T)
          ~match_:tab
          ~with_:(fun tab (local_ _graph) ->
            match tab with
            | A ->
              let%arr change_tab in
              Vdom.Node.button
                ~attrs:[ Vdom.Attr.on_click (fun _ -> change_tab T.C) ]
                [ Vdom.Node.text "jump to c" ]
            | B -> Bonsai.return (Vdom.Node.text "why are you even here")
            | C -> Bonsai.return (Vdom.Node.text "hello!"))
          graph)
      graph
  in
  let%arr contents in
  Tabs.Result.combine_trivially contents
;;

let () = Bonsai_web.Start.start component ~enable_bonsai_telemetry:Enabled
