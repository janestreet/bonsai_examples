open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

module Comment = struct
  type t =
    { id : int
    ; title : string
    ; content : string
    }
end

let all_comments_var =
  Bonsai.Expert.Var.create
    [ { Comment.id = 1; title = "Comment 1"; content = "This is comment 1" }
    ; { Comment.id = 2; title = "Comment 2"; content = "This is comment 2" }
    ; { Comment.id = 3; title = "Comment 3"; content = "This is comment 3" }
    ]
;;

let all_comments = Bonsai.Expert.Var.value all_comments_var

let comment_view ~close { Comment.title; content; _ } =
  {%html|
    <article>
      <h1>#{title}</h1>
      <p>#{content}</p>
      <button on_click=%{fun _ -> close}>x</button>
    </article>
  |}
;;

(* $MDX part-begin=storing_derived_values *)

let comments (local_ graph) =
  let selected_comment_view, set_selected_comment_view = Bonsai.state None graph in
  let comment_list =
    let%arr all_comments and set_selected_comment_view in
    List.map all_comments ~f:(fun comment ->
      let on_click _ =
        let close = set_selected_comment_view None in
        set_selected_comment_view (Some (comment_view ~close comment))
      in
      {%html|<li on_click=%{on_click}>#{comment.title}</li>|})
    |> Vdom.Node.ul
  in
  let comment_detail =
    let%arr selected_comment_view in
    Option.value selected_comment_view ~default:Vdom.Node.none
  in
  let%arr comment_list and comment_detail in
  Vdom.Node.div [ comment_list; comment_detail ]
;;

(* $MDX part-end *)

let trim_at_first_paren s =
  match String.substr_index s ~pattern:" (" with
  | Some idx -> String.sub s ~pos:0 ~len:idx
  | None -> s
;;

let with_updating_comments app =
  let likes = ref 0 in
  Async_kernel.Clock_ns.every (Time_ns.Span.of_sec 1.) (fun () ->
    incr likes;
    Bonsai.Expert.Var.update all_comments_var ~f:(fun comments ->
      List.map comments ~f:(fun comment ->
        { comment with
          content =
            trim_at_first_paren comment.content ^ [%string " (%{!likes#Int} likes)"]
        })));
  app
;;

let () = Util.run (with_updating_comments comments) ~id:"storing_derived_values"

(* $MDX part-begin=computing_derived_values *)

let comments (local_ graph) =
  let selected_comment_id, set_selected_comment_id = Bonsai.state None graph in
  let comment_list =
    let%arr all_comments and set_selected_comment_id in
    List.map all_comments ~f:(fun comment ->
      let on_click _ = set_selected_comment_id (Some comment.id) in
      {%html|<li on_click=%{on_click}>#{comment.title}</li>|})
    |> Vdom.Node.ul
  in
  let selected_comment =
    let%arr all_comments and selected_comment_id in
    let%bind.Option selected_comment_id in
    List.find all_comments ~f:(fun comment -> comment.id = selected_comment_id)
  in
  let comment_detail =
    match%sub selected_comment with
    | Some comment ->
      let%arr comment and set_selected_comment_id in
      comment_view ~close:(set_selected_comment_id None) comment
    | None -> Bonsai.return Vdom.Node.none
  in
  let%arr comment_list and comment_detail in
  Vdom.Node.div [ comment_list; comment_detail ]
;;

(* $MDX part-end *)
let () = Util.run (with_updating_comments comments) ~id:"computing_derived_values"
