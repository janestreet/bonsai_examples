open! Core
open! Bonsai_web

(* !!!TURN AWAY!!! *)
(* This is not good code!  It's made to interact in weird ways with docpub, and
   you shouldn't be looking here for inspiration.
   Of particular note:

   - The ui-components and call to "Bonsai.Start.start" are in the same file;
     they should be in separate files to encourage testability
   - Some code is written non-idiomatically because the ordering of the guide
     makes it undesierable to use concepts that weren't explained yet *)

module _ = Bonsai_types
module _ = Intro_examples
module _ = Vdom_examples
module _ = Incrementality_examples
module _ = State_examples
module _ = Effect_examples
module _ = Control_flow_examples

(* How-tos *)

(* Benchmarking_examples shows how to run benchmarks, so we don't want to link it in. *)
module _ = Bonsai_runtime_examples
module _ = Css_examples
module _ = Cutoff_examples
module _ = Best_practices_examples
module _ = Edge_triggered_examples
module _ = Dynamic_scope_examples
module _ = Effect_stale_examples
module _ = Focus_examples
module _ = Form_examples
module _ = Higher_order_examples
module _ = Javascript_interop_examples
module _ = Lifecycle_examples
module _ = Low_level_vdom_examples
module _ = Organizing_state_examples
module _ = Prt_examples
module _ = Rpc_examples
module _ = Scope_model_examples
module _ = State_reset_examples
module _ = Time_examples
module _ = Theming_examples
module _ = Url_var_examples

let () = Util.no_hash_fallback_must_run_last ()
