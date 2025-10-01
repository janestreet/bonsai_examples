open! Core
module Bonsai = Bonsai.Cont

let assoc = Bonsai.assoc

let state_machine ~default_model ~apply_action (local_ graph) =
  Bonsai.state_machine ~default_model ~apply_action graph
;;

let peek = Bonsai.peek

module Url_var = Bonsai_web_ui_url_var

let mirror = Bonsai_extra.Mirror.mirror

module Apply_action_context = Bonsai.Apply_action_context
