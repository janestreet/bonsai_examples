open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Seqnum = Bonsai_extra.Id_gen (Int) ()

module Model = struct
  type t =
    { seqnum : Seqnum.t option
    ; files : string Or_error.t Int.Map.t
    }

  let default = { seqnum = None; files = Int.Map.empty }

  let apply_action _ctx t (seqnum, id, content) =
    let t =
      if Option.equal Seqnum.equal t.seqnum (Some seqnum)
      then t
      else { seqnum = Some seqnum; files = Int.Map.empty }
    in
    { t with files = Map.set t.files ~key:id ~data:content }
  ;;
end

let component (local_ graph) =
  let seqnum = Seqnum.component graph in
  let image_state, inject_image_state =
    Bonsai.state_machine
      ~default_model:Model.default
      ~apply_action:Model.apply_action
      graph
  in
  let%tydi { drop_target; dragging_over } =
    Byo_file.on_drop
      ~mime_types:[ "image/png" ]
      ~f:
        (let%arr seqnum and inject_image_state in
         fun files ~files_not_matching_mime_type ->
           let all_files =
             List.map files ~f:Result.return
             @ List.map files_not_matching_mime_type ~f:(fun file -> Error file)
           in
           let%bind.Effect seqnum in
           List.mapi all_files ~f:(fun i -> function
             | Error not_a_png ->
               inject_image_state
                 ( seqnum
                 , i
                 , Error
                     (Error.of_string
                        (Js_of_ocaml.Js.to_string not_a_png##.name ^ " is not a png")) )
             | Ok file ->
               let file =
                 Bonsai_web_ui_file_from_web_file.create ~mode:`As_data_url file
               in
               (match%bind.Effect Bonsai_web_ui_file.contents file with
                | Ok contents ->
                  inject_image_state (seqnum, i, Ok (Bigstring.to_string contents))
                | Error e ->
                  Effect.print_s
                    [%message
                      "error reading"
                        ~file:(Bonsai_web_ui_file.filename file)
                        ~_:(e : Error.t)]))
           |> Effect.Many)
      graph
  in
  let%arr image_state and dragging_over and drop_target in
  let images =
    Map.map image_state.files ~f:(function
      | Ok url -> {%html|<img src=%{url} />|}
      | Error e -> Vdom.Node.sexp_for_debugging [%sexp (e : Error.t)])
  in
  let background =
    match dragging_over with
    | Some `All_files_valid -> "lightblue"
    | Some `Some_files_valid -> "pink"
    | Some `No_files_valid -> "red"
    | None -> "unset"
  in
  {%html|
    <div>
      <div
        %{drop_target}
        style="
          width: 100px;
          height: 100px;
          border: 1px solid black;
          background: %{background};
        "
      >
        Drop images here
      </div>
      %{Vdom.Node.Map_children.div (images)}
    </div>
  |}
;;

let () = Bonsai_web.Start.start component ~enable_bonsai_telemetry:Enabled
