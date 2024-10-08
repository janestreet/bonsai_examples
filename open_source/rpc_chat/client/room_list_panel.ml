open! Core
open! Bonsai_web
open Bonsai_chat_open_source_common
open Bonsai.Let_syntax

let component ~room_list ~refresh_rooms ~change_room _graph =
  let%arr room_list in
  let room_header =
    Vdom.Node.h2
      [ Vdom.Node.text "Rooms"
      ; Vdom_input_widgets.Button.simple
          ~merge_behavior:Legacy_dont_merge
          ~extra_attrs:[ Vdom.Attr.id "refresh-button" ]
          ~on_click:(fun () -> refresh_rooms)
          "↻"
      ]
  in
  let room_switching_buttons =
    List.map room_list ~f:(fun room ->
      Vdom_input_widgets.Button.simple
        ~merge_behavior:Legacy_dont_merge
        ~on_click:(fun () -> change_room room)
        (Room.to_string room))
  in
  Vdom.Node.div
    ~attrs:[ Vdom.Attr.id "room-list-panel" ]
    ([ room_header ] @ room_switching_buttons)
;;
