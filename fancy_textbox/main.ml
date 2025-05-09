open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Fancy = Bonsai_experimental_fancy_textbox

let component (local_ graph) =
  let text, set_text = Bonsai.state "this is a test\nof the fancy textarea" graph in
  let%arr text and set_text in
  Fancy.create () ~text ~set_text ~process:(fun text ->
    let words =
      String.split_on_chars text ~on:[ '\n'; '\t'; ' ' ]
      |> List.filter ~f:(Fn.non String.is_empty)
    in
    List.map words ~f:(fun s ->
      let color = if String.length s mod 2 = 0 then `Name "red" else `Name "blue" in
      s, Fancy.Decoration.create ~color ()))
;;

let growable_div_styles =
  {%css|
    border: 1px solid black;
    background: rgb(200, 200, 200);
    overflow: auto;
    resize: both;
    width: 400px;
    height: 400px;
    padding: 3px;
  |}
;;

let textbox_container_styles =
  {%css|
    border: 1px solid black;
    border-radius: 3px;
    padding: 0.25em 0.5em;
    background: white;
    min-width: 100px;
    width: fit-content;
    height: fit-content;
  |}
;;

let app graph =
  let%arr textbox = component graph in
  {%html|
    <div style="font-family: sans-serif">
      <div>
        This is a fancy textarea component. It has two fancy features:
        <ol>
          <li>
            <b>The textarea grows and shrinks to fit its content.</b> <br />
            Try adding new lines and long lines and then manipulate the size of the
            parent container to see how it responds to layout constraints
          </li>
          <li>
            <b>The user can arbitrarily colorize the content of the textarea.</b>
            <br />
            In this example, I color each word according to the number of
            non-whitespace chars in the word: red for an even number, blue for odd.
          </li>
        </ol>
      </div>
      <div %{growable_div_styles}>
        <div %{textbox_container_styles}>%{textbox}</div>
      </div>
    </div>
  |}
;;

let () = Bonsai_web.Start.start ~enable_bonsai_telemetry:Enabled app
