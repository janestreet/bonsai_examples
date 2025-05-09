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

type my_thing =
  { foo : int
  ; bar : string
  }

let do_something = Fn.id

let _ : int Bonsai.t =
  let my_thing = return { foo = 5; bar = "hello" } in
  let x =
    (* $MDX part-begin=let_arr_record_good *)
    let%arr { foo; _ } = my_thing in
    do_something foo
    (* $MDX part-end *)
  in
  x
;;

let _ : int Bonsai.t =
  let my_thing = return { foo = 5; bar = "hello" } in
  let x =
    (* $MDX part-begin=let_arr_record_bad *)
    let%arr my_thing in
    do_something my_thing.foo
    (* $MDX part-end *)
  in
  x
;;

let _ : int Bonsai.t =
  let my_thing = return { foo = 5; bar = "hello" } in
  let x =
    (* $MDX part-begin=let_map_record_bad *)
    let%map { foo; _ } = my_thing in
    do_something foo
    (* $MDX part-end *)
  in
  x
;;

(* $MDX part-begin=side_effect_let_arr_print *)
let print_on_change (a : int Bonsai.t) : int Bonsai.t =
  let%arr a in
  print_endline [%string "state is now %{a#Int}"];
  a
;;

(* $MDX part-end *)

let () = ignore print_on_change
let send_http_request = Fn.ignore

(* $MDX part-begin=side_effect_let_arr_send_query *)
let dispatch_query_on_change (a : int Bonsai.t) : int Bonsai.t =
  let%arr a in
  send_http_request [%string "https://example.com?query=%{a#Int}"];
  a
;;

(* $MDX part-end *)

let () = ignore dispatch_query_on_change

let get_document_title () =
  let open Js_of_ocaml in
  Dom_html.document##.title |> Js.to_string
;;

(* $MDX part-begin=side_effect_let_arr_compute_title *)
let compute_title (prefix : string Bonsai.t) (suffix : string Bonsai.t) : string Bonsai.t =
  let%arr prefix and suffix in
  (* https://developer.mozilla.org/en-US/docs/Web/API/Document/title *)
  let document_title = get_document_title () in
  [%string "%{prefix} %{document_title} %{suffix}"]
;;

(* $MDX part-end *)

module Big_data = String

(* $MDX part-begin=doing_work_to_compute_effect *)
let copy_to_clipboard_button (data : Big_data.t Bonsai.t) (label : string Bonsai.t)
  : Vdom.Node.t Bonsai.t
  =
  let on_click =
    let%arr data in
    let serialized_data = Big_data.sexp_of_t data |> Sexp.to_string in
    Byo_clipboard.copy_text serialized_data
  in
  let%arr on_click and label in
  Vdom.Node.button
    ~attrs:[ Vdom.Attr.on_click (fun _ -> on_click) ]
    [ Vdom.Node.text [%string "Copy: %{label}"] ]
;;

(* $MDX part-end *)

let () =
  let data =
    Bonsai.return (String.init 100 ~f:(fun _ -> Char.of_int_exn (Random.int 255)))
  in
  Util.run_vdom_val
    (copy_to_clipboard_button data (Bonsai.return "cool string"))
    ~id:"doing_work_to_compute_effect"
;;

(* $MDX part-begin=doing_work_as_part_of_effect *)
let copy_to_clipboard_button (data : Big_data.t Bonsai.t) (label : string Bonsai.t)
  : Vdom.Node.t Bonsai.t
  =
  let on_click =
    let%arr data in
    let%bind.Effect serialized_data =
      Effect.of_thunk (fun () -> Big_data.sexp_of_t data |> Sexp.to_string)
    in
    Byo_clipboard.copy_text serialized_data
  in
  let%arr on_click and label in
  Vdom.Node.button
    ~attrs:[ Vdom.Attr.on_click (fun _ -> on_click) ]
    [ Vdom.Node.text [%string "Copy: %{label}"] ]
;;

(* $MDX part-end *)

let () =
  let data =
    Bonsai.return (String.init 100 ~f:(fun _ -> Char.of_int_exn (Random.int 255)))
  in
  Util.run_vdom_val
    (copy_to_clipboard_button data (Bonsai.return "cool string"))
    ~id:"doing_work_as_part_of_effect"
;;

let () = ignore compute_title
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
