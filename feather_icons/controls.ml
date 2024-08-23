open! Core
open! Import

type t =
  { size : int
  ; stroke_width : float
  ; stroke : [ `Hex of string ]
  ; fill : [ `Hex of string ] option
  }
[@@deriving sexp, fields ~iterators:to_list]

let default = { size = 24; stroke_width = 2.; stroke = `Hex "#000000"; fill = None }

module Range =
  [%css
  stylesheet
    {|
      .class_ {
        height: 4px;
        cursor: pointer;
        appearance: none;
        background-color: #d1d5da;
        width: 100%;
      }
      |}]

let size_slider =
  Form.Elements.Range.int
    ~extra_attrs:(Bonsai.return [ Range.class_ ])
    ~min:12
    ~max:100
    ~default:default.size
    ~step:8
    ~allow_updates_when_focused:`Never
    ()
;;

let stroke_width_slider =
  Form.Elements.Range.float
    ~extra_attrs:(Bonsai.return [ Range.class_ ])
    ~min:0.5
    ~max:3.
    ~default:default.stroke_width
    ~step:0.25
    ~allow_updates_when_focused:`Never
    ()
;;

module Color_input =
  [%css
  stylesheet
    {|
      .class_ {
        cursor: pointer;
        height: 3em;
        width: 100%;
      }
      |}]

let display_none = Vdom.Attr.style (Css_gen.display `None)

let color_input ?(display = Bonsai.return true) () (local_ graph) =
  let classes_ = Vdom.Attr.many [ Card_like.class_; Color_input.class_ ] in
  let extra_attr =
    let%arr display in
    if display then classes_ else Vdom.Attr.(classes_ @ display_none)
  in
  Form.Elements.Color_picker.hex ~extra_attr () graph
;;

module Style =
  [%css
  stylesheet
    {|
      .header {
        display: flex;
        justify-content: space-between;
        align-items: center;
      }

      .reset {
        cursor: pointer;
      }

      .label {
        font-size: 14px;
      }

      .control_container {
        display: flex;
        flex-direction: column;
        gap: 8px;
      }

      .controls {
        display: flex;
        flex-direction: column;
        align-items: stretch;
        flex: 0 0 200px;
        gap: 16px;
      }
      |}]

module Fill = struct
  type t =
    { value : [ `Hex of string ] option
    ; reset : unit Effect.t
    ; view : Vdom.Node.t
    }

  module Fill =
    [%css
    stylesheet
      {|
        .class_ {
          display: flex;
          justify-content: space-between;
        }
        |}]

  let component : local_ Bonsai.graph -> t Bonsai.t =
    (* Equal to --js-primary-color *)
    fun (local_ graph) ->
    let default_fill_color = `Hex "#2085ef" in
    let fill_toggle = Form.Elements.Toggle.bool ~default:false () graph in
    let fill_on =
      let%arr fill_toggle in
      Form.value_or_default fill_toggle ~default:false
    in
    let fill_input =
      let form = color_input ~display:fill_on () graph in
      Form.Dynamic.with_default (Bonsai.return default_fill_color) form graph
    in
    let%arr fill_toggle and fill_on and fill_input in
    let value =
      match fill_on with
      | false -> None
      | true ->
        (match Form.value fill_input with
         | Error _ -> default.fill
         | Ok color -> Some color)
    in
    let view =
      Vdom.Node.div
        ~attrs:[ Style.control_container ]
        (Vdom.Node.div
           ~attrs:[ Fill.class_ ]
           [ Vdom.Node.label ~attrs:[ Style.label ] [ Vdom.Node.text "Fill" ]
           ; Form.view_as_vdom fill_toggle
           ]
         :: (Form.view fill_input |> Form.View.to_vdom_plain))
    in
    let reset =
      Effect.Many [ Form.set fill_toggle false; Form.set fill_input default_fill_color ]
    in
    { value; reset; view }
  ;;
end

let component (local_ graph) =
  let size_slider = size_slider graph in
  let stroke_width_slider = stroke_width_slider graph in
  let stroke_input = color_input () graph in
  let fill = Fill.component graph in
  let%arr size_slider
  and stroke_width_slider
  and stroke_input
  and { value = fill; reset = reset_fill; view = fill_view } = fill in
  (* We could have several of these forms using Form.Typed.Record, but
     refrained in order to retain control over the UI layout. *)
  let t =
    let size = Form.value_or_default size_slider ~default:default.size in
    let stroke_width =
      Form.value_or_default stroke_width_slider ~default:default.stroke_width
    in
    let stroke = Form.value_or_default stroke_input ~default:default.stroke in
    { size; stroke_width; stroke; fill }
  in
  let reset =
    Vdom.Node.button
      ~attrs:
        [ Vdom.Attr.many [ Card_like.class_; Style.reset ]
        ; Vdom.Attr.on_click (fun _ ->
            let { size; stroke_width; stroke; fill = _ } = default in
            Fields.to_list
              ~size:(fun _ -> Form.set size_slider size)
              ~stroke_width:(fun _ -> Form.set stroke_width_slider stroke_width)
              ~stroke:(fun _ -> Form.set stroke_input stroke)
              ~fill:(fun _ -> reset_fill)
            |> Effect.Many)
        ]
      [ Vdom.Node.text "Reset" ]
  in
  let header =
    let title = Vdom.Node.h3 [ Vdom.Node.text "Controls" ] in
    Vdom.Node.div ~attrs:[ Style.header ] [ title; reset ]
  in
  let control ~form ~label =
    Vdom.Node.div
      ~attrs:[ Style.control_container ]
      (Vdom.Node.label ~attrs:[ Style.label ] [ Vdom.Node.text label ]
       :: (Form.view form |> Form.View.to_vdom_plain))
  in
  let size = control ~form:size_slider ~label:(sprintf "Size: %dpx" t.size) in
  let stroke_width =
    control ~form:stroke_width_slider ~label:(sprintf "Stroke width: %gpx" t.stroke_width)
  in
  let stroke = control ~form:stroke_input ~label:"Stroke" in
  let view =
    Vdom.Node.div
      ~attrs:[ Style.controls ]
      [ header; size; stroke_width; stroke; fill_view ]
  in
  t, view
;;
