open! Core
open Bonsai_web
open Bonsai.Let_syntax

(* An example of a small Bonsai component for creating counter
   widgets ("+" and "-" buttons allow you to increment and decrement a
   number). The state of each counter is housed in a shared [Map]. Adding
   a new counter is as simple as adding a value to the map. *)

let component graph =
  let state, inject =
    Bonsai.state_machine
      ~default_model:Int.Map.empty
      ~apply_action:(fun _ctx model -> function
        | `New_counter -> Map.add_exn model ~key:(Map.length model) ~data:0
        | `Incr_by (counter_id, diff) ->
          Map.update model counter_id ~f:(function
            | None -> diff
            | Some x -> x + diff))
      graph
  in
  let%arr state and inject in
  let button text action =
    Vdom.Node.button
      ~attrs:[ Vdom.Attr.on_click (fun _ -> inject action) ]
      [ Vdom.Node.text text ]
  in
  let add_button = button "add" `New_counter in
  let for_each (i, c) =
    Vdom.Node.div
      [ button "-1" (`Incr_by (i, -1))
      ; Vdom.Node.textf "%d" c
      ; button "+1" (`Incr_by (i, 1))
      ]
  in
  let counters = state |> Map.to_alist |> List.map ~f:for_each in
  Vdom.Node.div (add_button :: counters)
;;

let () = Bonsai_web.Start.start component
