open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Ll_vdom = Bonsai_web_ui_low_level_vdom

module Canvas = struct
  open Js_of_ocaml

  type element = Dom_html.canvasElement

  type input =
    { width : int
    ; height : int
    }
  [@@deriving sexp_of]

  type state =
    { canvas : Canvas2d.Canvas.t
    ; ctx : Canvas2d.Ctx2d.t
    }

  let render ~(state : state) ~(input : input) =
    let _ : _ = input in
    let open Canvas2d in
    let canvas_width = Canvas.width state.canvas in
    let canvas_height = Canvas.height state.canvas in
    let cx = canvas_width / 2 in
    let cy = canvas_height / 2 in
    let max_distance = Float.of_int (Int.min cx cy) in
    Ctx2d.clear ~w:canvas_width ~h:canvas_height state.ctx;
    for x = 0 to canvas_width - 1 do
      for y = 0 to canvas_height - 1 do
        let dx = Float.of_int (x - cx) in
        let dy = Float.of_int (y - cy) in
        let distance = Float.sqrt ((dx *. dx) +. (dy *. dy)) in
        let distance_from_max = max_distance -. distance in
        let angle = Float.atan2 dy dx in
        let hue = angle in
        let saturation = Float.min 1.0 (distance /. max_distance) in
        let oklch =
          let l = 0.65 in
          let c = saturation *. 0.365 in
          let h = hue in
          Oklab.Lch.create ~l ~c ~h ()
        in
        let oklab = Oklab.Lch.to_lab oklch in
        if (* Oklab.inside_rgb oklab *)
           Float.(distance_from_max >= 1.0)
        then (
          let alpha = Float.clamp_exn ~min:0.0 ~max:1.0 (distance_from_max -. 1.0) in
          let oklab = Oklab.set_alpha oklab ~alpha in
          Ctx2d.set_fill_style state.ctx (Js.string (Oklab.to_string_css oklab));
          Ctx2d.fill_rect state.ctx ~x:(Float.of_int x) ~y:(Float.of_int y) ~w:1.0 ~h:1.0)
      done
    done
  ;;

  let init ~get_input:_ ~other_instances:_ ({ width; height } as input) =
    let canvas = Canvas2d.Canvas.create ~width ~height in
    let ctx = Canvas2d.Canvas.ctx2d ~color_space:`display_p3 canvas in
    let element = Canvas2d.Canvas.dom_element canvas in
    let state = { canvas; ctx } in
    render ~state ~input;
    state, element
  ;;

  let update ~other_instances:_ ~prev_input:_ input state element =
    render ~state ~input;
    element
  ;;

  let destroy ~other_instances:_ _input _state _element = ()
end

let component (local_ graph) =
  let input = Bonsai.return { Canvas.width = 255; height = 255 } in
  let canvas = Ll_vdom.Widget.component (module Canvas) input graph in
  let%arr canvas in
  canvas.view
;;

let () =
  Bonsai_web.Start.start
    (View.Theme.set_for_app (Bonsai.return (Kado.theme ~version:Bleeding ())) component)
    ~enable_bonsai_telemetry:Enabled
;;
