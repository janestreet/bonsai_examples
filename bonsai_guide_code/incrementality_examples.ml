open! Core
open! Async_kernel
open! Bonsai_web
open Bonsai.Let_syntax

(* $MDX part-begin=int_view *)
let int_view (a : int Bonsai.t) : Vdom.Node.t Bonsai.t =
  let%arr (a : int) = (a : int Bonsai.t) in
  Vdom.Node.div [ Vdom.Node.text (Int.to_string a) ]
;;

(* $MDX part-end *)

let () = Util.run_vdom_val (int_view (Bonsai.return 5)) ~id:"int_view"

(* $MDX part-begin=sum_and_display *)
let sum_and_display (a : int Bonsai.t) (b : int Bonsai.t) : Vdom.Node.t Bonsai.t =
  let%arr a and b in
  Vdom.Node.textf "%d + %d = %d" a b (a + b)
;;

(* $MDX part-end *)

let () =
  Util.run_vdom_val
    (sum_and_display (Bonsai.return 5) (Bonsai.return 8))
    ~id:"sum_and_display"
;;

let expensive_calculate_exponent ~risk_parameter = Float.sqrt risk_parameter

(* $MDX part-begin=analyze_list_inefficient *)
let analyze_list (big_list : float list) (risk_parameter : float) : float =
  List.fold big_list ~init:0. ~f:(fun sum x ->
    sum +. (x ** expensive_calculate_exponent ~risk_parameter))
;;

(* $MDX part-end *)
let () = ignore analyze_list

(* $MDX part-begin=analyze_list_efficient *)
let analyze_list (big_list : float list) (risk_parameter : float) : float =
  let exponent = expensive_calculate_exponent ~risk_parameter in
  List.fold big_list ~init:0. ~f:(fun sum x -> sum +. (x ** exponent))
;;

(* $MDX part-end *)

let () = ignore analyze_list

(* $MDX part-begin=incremental_f_inefficient *)
let exp_and_divide (a : float Bonsai.t) (b : float Bonsai.t) (c : float Bonsai.t) =
  let%arr a and b and c in
  (a ** b) /. c
;;

(* $MDX part-end *)
let () = ignore exp_and_divide

(* $MDX part-begin=incremental_f_efficient *)
let exp_and_divide (a : float Bonsai.t) (b : float Bonsai.t) (c : float Bonsai.t) =
  let dividend =
    let%arr a and b in
    a ** b
  in
  let%arr dividend and c in
  dividend /. c
;;

(* $MDX part-end *)

let () = ignore exp_and_divide
