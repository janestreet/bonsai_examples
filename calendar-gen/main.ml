open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Form = Bonsai_web_ui_form.With_manual_view

let days_until_saturday ~year ~month =
  let rec loop ~day acc =
    match Date.day_of_week day with
    | Sat -> acc
    | _ -> loop ~day:(Date.add_days day 1) (acc + 1)
  in
  loop ~day:(Date.create_exn ~y:year ~m:month ~d:1) 0
;;

module _ =
  [%css
  stylesheet
    {|
      * {
        box-sizing: border-box;
        font-family: "Source Code Pro", monospace;
      }
    |}]

let box_css =
  {%css|
    min-width: 100px;
    min-height: 100px;

    max-width: 100px;
    max-height: 100px;

    position: relative;

    & > div {
      border: 1px solid black;
      width: calc(100% + 1px);
      height: calc(100% + 1px);
    }
  |}
;;

let month_name_box =
  {%css|
    max-width: 100px;
    min-width: 100px;
    font-size: 2em;
    font-weight: bold;
    display: flex;
    justify-content: center;
    align-items: center;
  |}
;;

let render ~year =
  let max_days_until_saturday =
    List.range ~start:`inclusive ~stop:`inclusive 1 12
    |> List.map ~f:(fun month ->
      days_until_saturday ~year ~month:(Month.of_int_exn month))
    |> List.max_elt ~compare:Int.compare
    |> Option.value_exn
  in
  let render_month ~month =
    let month = Month.of_int_exn month in
    let date_boxes =
      List.range ~start:`inclusive ~stop:`inclusive 1 (Date.days_in_month ~year ~month)
      |> List.map ~f:(fun day_number ->
        let day = Date.create_exn ~y:year ~m:month ~d:day_number in
        let weekend_attrs =
          if Date.is_weekend day then {%css|background: #e7fdff;|} else {%css||}
        in
        let attrs = {%css||} in
        {%html|
          <div %{weekend_attrs} %{attrs} %{box_css}>
            <div>
              <div style="padding: 0.5em; position: absolute; right: 0">
                #{Int.to_string day_number}
              </div>
            </div>
          </div>
        |})
    in
    let spacing_boxes =
      let spacer_count = max_days_until_saturday - days_until_saturday ~year ~month in
      let spacer_count = if spacer_count = 0 then 7 else spacer_count in
      let spacer_count = spacer_count - 1 in
      List.range ~start:`exclusive ~stop:`inclusive 0 spacer_count
      |> List.map ~f:(fun _ -> {%html|<div %{box_css}></div>|})
    in
    let boxes = spacing_boxes @ date_boxes in
    let view =
      View.hbox
        ({%html|<div %{month_name_box}>#{ Month.to_string month |> String.lowercase }</div>|}
         :: boxes)
    in
    view, List.length boxes
  in
  let months, month_lengths =
    List.range ~start:`inclusive ~stop:`inclusive 1 12
    |> List.map ~f:(fun month -> render_month ~month)
    |> List.unzip
  in
  let week_day_names =
    List.range
      ~start:`inclusive
      ~stop:`exclusive
      0
      (List.max_elt month_lengths ~compare:Int.compare |> Option.value_exn)
    |> List.map ~f:(fun i ->
      let day_name =
        match i % 7 with
        | 0 -> "mon"
        | 1 -> "tue"
        | 2 -> "wed"
        | 3 -> "thu"
        | 4 -> "fri"
        | 5 -> "sat"
        | 6 -> "sun"
        | _ -> assert false
      in
      {%html|
        <div
          %{box_css}
          style="
            font-size: 1.5em;
            font-weight: bold;
            display: flex;
            justify-content: center;
            align-items: flex-end;
            padding-bottom: 0.5em;
          "
        >
          #{day_name}
        </div>
      |})
    |> (fun day_names -> {%html|<div %{month_name_box}></div>|} :: day_names)
    |> View.hbox
  in
  View.vbox ~attrs:[ {%css|margin: 2em;|} ] (week_day_names :: months)
;;

let component (local_ graph) =
  let year_picker = Form.Elements.Number.int ~default:2024 ~step:1 () graph in
  let%arr year_picker in
  let year = Form.value_or_default ~default:2024 year_picker in
  View.hbox
    [ View.vbox [ Form.view year_picker; render ~year ]
    ; {%html|<div style="width: 20px"></div>|}
    ]
;;

let () = Bonsai_web.Start.start ~enable_bonsai_telemetry:Enabled component
