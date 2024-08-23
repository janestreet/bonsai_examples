open! Core
open! Bonsai_web
open Bonsai_chat_open_source_common

val component
  :  messages:Message.t list Bonsai.t
  -> current_room:Room.t Bonsai.t
  -> local_ Bonsai.graph
  -> Vdom.Node.t Bonsai.t
