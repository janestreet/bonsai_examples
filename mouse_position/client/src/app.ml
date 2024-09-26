open! Core
open Bonsai_web
open Bonsai_examples_mouse_position_common
open Bonsai.Let_syntax
open Username_kernel
open Vdom
module View = Bonsai_web_ui_view

let colors =
  Array.of_list_map Tailwind_colors.Hue.all ~f:(fun hue ->
    Tailwind_colors.create hue `_300)
;;

let session_to_color session =
  [ Attr.style
      (Css_gen.background_color colors.(Session.to_int_exn session % Array.length colors))
  ]
;;

let app graph =
  let%sub { last_ok_response; _ } =
    Rpc_effect.Polling_state_rpc.poll
      ~sexp_of_query:[%sexp_of: Unit.t]
      ~sexp_of_response:[%sexp_of: Active_users.t]
      ~equal_query:[%equal: Unit.t]
      ~equal_response:[%equal: Active_users.t]
      Protocol.Active_users.rpc
      ~where_to_connect:Self
      ~every:(Time_ns.Span.of_sec 1.0)
      (Bonsai.return ())
      graph
  in
  let active_users =
    match%arr last_ok_response with
    | Some (_, { active_users }) -> active_users
    | None -> Session.Map.empty
  in
  let rpc_results =
    Bonsai.assoc
      (module Session)
      active_users
      ~f:(fun session username graph ->
        let result =
          Rpc_effect.Rpc.poll
            ~sexp_of_query:[%sexp_of: Session.t]
            ~sexp_of_response:[%sexp_of: Mouse_position.t option]
            ~equal_query:[%equal: Session.t]
            ~equal_response:[%equal: Mouse_position.t option]
            Protocol.Get_mouse_position.rpc
            ~where_to_connect:Self
            ~every:(Time_ns.Span.of_sec 0.1)
            session
            graph
        in
        let%arr result and username in
        result, username)
      graph
  in
  let mouse_positions =
    Bonsai.Map.filter_mapi
      rpc_results
      ~f:(fun ~key:_ ~data:({ last_ok_response; _ }, username) ->
        match last_ok_response with
        | Some (_, Some mouse_position) -> Some (mouse_position, username)
        | None | Some (_, None) -> None)
      graph
  in
  let cursor_blocks =
    Bonsai.assoc
      (module Session)
      mouse_positions
      ~f:(fun session info _graph ->
        let%arr session
        and mouse_position, username = info in
        Node.div
          ~key:(Session.to_string session)
          ~attrs:
            [ Attr.style
                Css_gen.(
                  position
                    ~top:(`Px mouse_position.y)
                    ~left:(`Px mouse_position.x)
                    `Absolute)
            ; Vdom.Attr.many (session_to_color session)
            ; Style.item
            ]
          [ Node.text (Username.to_string username) ])
      graph
  in
  let status_sidebar =
    let theme = View.Theme.current graph in
    let%arr rpc_results and theme in
    let rows = Map.to_alist rpc_results in
    let columns =
      [ View.Table.Col.make
          "Session"
          ~get:(fun (session, _) -> session)
          ~render:(fun _ session -> Vdom.Node.text (Session.to_string session))
          ~cell_attrs:session_to_color
      ; View.Table.Col.make
          "Username"
          ~get:(fun (_, (_, username)) -> username)
          ~render:(fun _ username -> Vdom.Node.text (Username.to_string username))
      ; View.Table.Col.make
          "Coordinates"
          ~get:(fun (_, ({ Rpc_effect.Poll_result.last_ok_response; _ }, _)) ->
            last_ok_response)
          ~render:(fun _ last_ok_response ->
            match last_ok_response with
            | Some (_, Some mouse_position) ->
              Vdom.Node.textf "%d x %d" mouse_position.Mouse_position.x mouse_position.y
            | None | Some (_, None) -> Vdom.Node.text "Unknown")
      ]
    in
    Vdom.Node.div ~attrs:[ Style.sidebar ] [ View.Table.render theme columns rows ]
  in
  let set_mouse_position =
    Rpc_effect.Rpc.dispatcher Protocol.Set_mouse_position.rpc ~where_to_connect:Self graph
  in
  let%arr cursor_blocks and status_sidebar and set_mouse_position in
  Node.div
    ~attrs:
      [ Style.container
      ; Attr.on_mousemove (fun event ->
          let x = event##.clientX in
          let y = event##.clientY in
          Effect.ignore_m (set_mouse_position { x; y }))
      ]
    (status_sidebar :: Map.data cursor_blocks)
;;
