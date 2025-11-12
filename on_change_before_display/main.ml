open! Core
open! Bonsai_web
open Bonsai.Let_syntax

let block =
  {%css|
    width: 50px;
    height: 50px;
  |}
;;

let red = {%css|background: red;|}
let blue = {%css|background: blue;|}

let mono_td text =
  {%html|<td style="font-family: monospace; padding: 2px 5px">*{text}</td>|}
;;

let state_synced_description =
  {%html|
    <p>
      The first row uses Bonsai.Edge.on_change to sync its state with the counter.
      If its state does not equal the counter, it shows a red square.
    </p>
  |}
;;

let state_synced_test input ~trigger graph =
  let state, set_state = Bonsai.state 0 graph in
  Bonsai.Edge.on_change ~trigger input ~equal:equal_int ~callback:set_state graph;
  let%arr input and state in
  let attrs = if input = state then [ block ] else [ block; red ] in
  {%html|<div *{attrs}></div>|}
;;

let state_transition_description =
  {%html|
    <p>
      The second row uses Bonsai.Edge.on_change to reset its state to 0 when the
      counter changes. When its state is 0, it shows a red square, and uses
      lifecycles to set the state to 1. When its state is 1, it shows a blue square,
      and uses lifecycles to set the state to 2.
    </p>
  |}
;;

let state_transition_test input ~trigger graph =
  let state, set_state = Bonsai.state 0 graph in
  Bonsai.Edge.on_change
    ~trigger
    input
    ~equal:equal_int
    ~callback:
      (let%arr set_state in
       fun _ -> set_state 0)
    graph;
  let on_display effect graph =
    match trigger with
    | `Before_display -> Bonsai.Edge.lifecycle ~before_display:effect graph
    | `After_display -> Bonsai.Edge.lifecycle ~after_display:effect graph
  in
  let attrs =
    match%sub state with
    | 0 ->
      on_display
        (let%arr set_state in
         set_state 1)
        graph;
      Bonsai.return [ block; red ]
    | 1 ->
      on_display
        (let%arr set_state in
         set_state 2)
        graph;
      Bonsai.return [ block; blue ]
    | _ -> Bonsai.return [ block ]
  in
  let%arr attrs in
  {%html|<div *{attrs}></div>|}
;;

let component (local_ graph) =
  let state, set_state = Bonsai.state 0 graph in
  let before_1 = state_synced_test state ~trigger:`Before_display graph in
  let after_1 = state_synced_test state ~trigger:`After_display graph in
  let before_2 = state_transition_test state ~trigger:`Before_display graph in
  let after_2 = state_transition_test state ~trigger:`After_display graph in
  let%arr state and set_state and before_1 and after_1 and before_2 and after_2 in
  {%html|
    <div>
      %{state_synced_description} %{state_transition_description}

      <button on_click=%{fun _ -> set_state (state + 1)}>
        incr: %{state#Int}
      </button>
      <table
        style="
          border-collapse: collapse;
          margin-top: 3px;

          & td {
            border: 1px solid black;
          }
        "
      >
        <thead>
          <tr>
            <%{mono_td}>~trigger:`Before_display</>
            <%{mono_td}>~trigger:`After_display</>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td>%{before_1}</td>
            <td>%{after_1}</td>
          </tr>
          <tr>
            <td>%{before_2}</td>
            <td>%{after_2}</td>
          </tr>
        </tbody>
      </table>
    </div>
  |}
;;

let () = Bonsai_web.Start.start ~use_new_experimental_implementation:true component
