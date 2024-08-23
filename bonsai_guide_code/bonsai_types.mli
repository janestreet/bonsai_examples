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
  -> ('k, 'cmp) Bonsai.comparator
  -> ('k, 'v, 'cmp) Map.t Bonsai.t
  -> f:('k Bonsai.t -> 'v Bonsai.t -> local_ Bonsai.graph -> 'result Bonsai.t)
  -> local_ Bonsai.graph
  -> ('k, 'result, 'cmp) Map.t Bonsai.t
(* $MDX part-end *)

(* $MDX part-begin=state_machine0 *)
val state_machine0
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
      :  ?on_fallback_raises:'a
      -> ?encoding_behavior:Uri_parsing.Percent_encoding_behavior.t
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

(** [mirror] is used to reflect state back and forth between locations.
    Frequently this will be used to back up a components model in a more
    persistent form of storage, such as the URL, or local-storage.

    The gist of this combinator is that if you have two states that you'd
    like to be synchronized, you can feed the "current value" and "set
    value" functions for both states into [mirror] and they'll
    automatically be kept up to date. Either of these can be backed by any
    kind of structure, but there are some important differences in their
    symmetry.

    When the component is first loaded, [store] has priority, so if the
    values are different, [store] wins, and [interactive] has its value
    "set". From that point on, if either incoming value changes, the
    opposite setter is called. In the case that both [store] and
    [interactive] change at the same time, the tie is broken in favor of
    [interactive], and [store_set] is called. *)
val mirror
  :  ?sexp_of_model:('m -> Sexp.t)
  -> equal:('m -> 'm -> bool)
  -> store_set:('m -> unit Effect.t) Bonsai.t
  -> store_value:'m Bonsai.t
  -> interactive_set:('m -> unit Effect.t) Bonsai.t
  -> interactive_value:'m Bonsai.t
  -> unit
  -> local_ Bonsai.graph
  -> unit Bonsai.t
(* $MDX part-end *)
