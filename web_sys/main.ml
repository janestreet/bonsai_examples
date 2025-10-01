open Browser

let () =
  let window =
    match Global.get () with
    | `Window w -> w
    | _ -> assert false
  in
  let document = Window.get_document window in
  let body = Document.get_body document in
  let text_node = Document.create_text_node document ~data:"hello world" in
  match body with
  | Some body ->
    let _ : Node.t = Node.append_child body ~node:text_node in
    ()
  | None -> ()
;;
