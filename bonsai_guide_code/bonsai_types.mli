(* A collection of bonsai type signatures that we'd like to stay up-to-date in the guide
   code. *)

open! Core
module Bonsai := Bonsai.Cont
module Effect := Bonsai.Effect
module Computation_status := Bonsai.Computation_status
module Url_var := Bonsai_web_ui_url_var

(* $MDX part-begin=assoc *)
val assoc
  :  here:[%call_pos]
  -> ('k, 'cmp) Comparator.Module.t
  -> ('k, 'v, 'cmp) Map.t Bonsai.t
  -> f:('k Bonsai.t -> 'v Bonsai.t -> local_ Bonsai.graph -> 'result Bonsai.t)
  -> local_ Bonsai.graph
  -> ('k, 'result, 'cmp) Map.t Bonsai.t
(* $MDX part-end *)

(* $MDX part-begin=state_machine *)
val state_machine
  :  default_model:'model
  -> apply_action:
       (('action, unit) Bonsai.Apply_action_context.t -> 'model -> 'action -> 'model)
  -> local_ Bonsai.graph
  -> 'model Bonsai.t * ('action -> unit Effect.t) Bonsai.t
(* $MDX part-end *)

(* $MDX part-begin=peek *)
val peek
  :  here:[%call_pos]
  -> 'a Bonsai.t
  -> local_ Bonsai.graph
  -> 'a Computation_status.t Effect.t Bonsai.t
(* $MDX part-end *)

module Url_var : sig
  (* $MDX part-begin=url_var_components *)
  module Components : sig
    type t =
      { path : string
      ; query : string list String.Map.t
      ; fragment : string option
      }
  end

  (* $MDX part-end *)
  (* $MDX part-begin=url_var_from_handwritten *)
  module type T = sig
    type t [@@deriving sexp, equal]
  end

  module type S = sig
    include T

    val parse_exn : Components.t -> t
    val unparse : t -> Components.t
  end

  val create_exn : (module S with type t = 'a) -> fallback:'a -> 'a Url_var.t
  (* $MDX part-end *)

  (* $MDX part-begin=url_var_from_uri_parsing *)
  module Typed : sig
    val make
      :  ?navigation:[ `Ignore | `Intercept ]
      -> ?on_fallback_raises:'a
      -> ?encoding_behavior:Uri_parsing.Percent_encoding_behavior.t
      -> ?trailing_slash_behavior:Uri_parsing.Trailing_slash_behavior.t
      -> (module T with type t = 'a)
      -> 'a Uri_parsing.Versioned_parser.t
      -> fallback:(Exn.t -> Components.t -> 'a)
      -> 'a Url_var.t
  end
  (* $MDX part-end *)

  (* $MDX part-begin=url_var_usage_api *)
  val value : 'a Url_var.t -> 'a Bonsai.t
  val set_effect : ?how:[ `Push | `Replace ] -> 'a Url_var.t -> 'a -> unit Effect.t
  (* $MDX part-end *)
end

(* $MDX part-begin=mirror *)
val mirror
  :  ?sexp_of_model:('m -> Sexp.t)
  -> equal:('m -> 'm -> bool)
  -> store_set:('m -> unit Effect.t) Bonsai.t
  -> store_value:'m Bonsai.t
  -> interactive_set:('m -> unit Effect.t) Bonsai.t
  -> interactive_value:'m Bonsai.t
  -> local_ Bonsai.graph
  -> unit
(* $MDX part-end *)

(* $MDX part-begin=apply_action_context *)
module Apply_action_context : sig
  (** A value with the type [('action, 'response) Apply_action_context.t] is provided to
      all state-machine's [apply_action] functions. It can be used to do a variety of
      things that are only legal inside of [apply_action]:
      1. Access the application time source directly. This is most likely useful to read
         the current time or sleep for some time span
      2. "inject" a value corresponding to the state-machine's action type into an effect
         that can be scheduled.
      3. Directly schedule effects. *)

  type ('action, 'response) t

  val inject : ('action, 'response) t -> 'action -> 'response Effect.t
  val schedule_event : _ t -> unit Effect.t -> unit
  val time_source : _ t -> Bonsai.Time_source.t
end
(* $MDX part-end *)
