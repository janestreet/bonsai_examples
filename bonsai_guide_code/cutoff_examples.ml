open! Core
open! Async_kernel
open! Bonsai_web
open! Bonsai.Let_syntax

let expensive_combine a b = a + b

(* $MDX part-begin=mod_cutoff *)
let computation (a : int Bonsai.t) (b : int Bonsai.t) : int Bonsai.t =
  let mod_two_a =
    let%arr a in
    a mod 2
  in
  let mod_three_b =
    let%arr b in
    b mod 3
  in
  let%arr mod_two_a and mod_three_b in
  expensive_combine mod_two_a mod_three_b
;;

(* $MDX part-end *)

let () = ignore computation
let derive_secret = Fn.id

module Secret = Int

(* $MDX part-begin=custom_cutoff *)
let computation (a : int Bonsai.t) (b : int Bonsai.t) : int Bonsai.t =
  let secret_a =
    (let%arr a in
     derive_secret a)
    |> Bonsai.cutoff ~equal:[%equal: Secret.t]
  in
  let secret_b =
    (let%arr b in
     derive_secret b)
    |> Bonsai.cutoff ~equal:[%equal: Secret.t]
  in
  let%arr secret_a and secret_b in
  expensive_combine secret_a secret_b
;;

(* $MDX part-end *)

let () = ignore computation

(* $MDX part-begin=last_modified_time *)
let with_last_modified_time
  ~equal
  input
  (* Although [Bonsai.Clock.Expert.now] is generally discouraged, the cutoff only pays
     attention to [input], so [now] shouldn't cause re-firing of this computation's
     transitive dependencies. *)
  (local_ graph)
  =
  let now = Bonsai.Clock.Expert.now graph in
  let result = Bonsai.both input now in
  let%sub result, time =
    Bonsai.Incr.value_cutoff result ~equal:(fun (a, _) (b, _) -> equal a b) graph
  in
  result, time
;;

(* $MDX part-end *)

let () = ignore with_last_modified_time
