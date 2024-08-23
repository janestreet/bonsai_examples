open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Animation = Bonsai_experimental_animation
module Form = Bonsai_web_ui_form.With_automatic_view

let component (local_ graph) =
  let interpolator_form =
    Form.Elements.Dropdown.enumerable
      (module Animation.Interpolator)
      ~init:`First_item
      graph
  in
  let text_picker =
    Form.Elements.Textbox.string ~allow_updates_when_focused:`Never () graph
  in
  let text_picker =
    Form.Dynamic.with_default (Bonsai.return "Hello Animation!") text_picker graph
  in
  let interpolator =
    interpolator_form >>| Form.value_or_default ~default:Animation.Interpolator.Linear
  in
  let%sub { value; animate } =
    Animation.Advanced.make
      ~fallback:(Bonsai.return 0.0)
      ~interpolate:Animation.Interpolatable.float
      graph
  in
  let forward, set_forward =
    Bonsai.state true ~sexp_of_model:[%sexp_of: Bool.t] ~equal:[%equal: Bool.t] graph
  in
  let get_forward = Bonsai.peek forward graph in
  let get_interpolator = Bonsai.peek interpolator graph in
  let get_things_started =
    let%arr animate and get_forward and get_interpolator and set_forward in
    let rec switch_directions () =
      let%bind.Effect forward =
        match%bind.Effect get_forward with
        | Active forward -> Effect.return forward
        | Inactive -> Effect.never
      in
      let%bind.Effect interpolator =
        match%bind.Effect get_interpolator with
        | Active interpolator -> Effect.return interpolator
        | Inactive -> Effect.never
      in
      let%bind.Effect () = set_forward (not forward) in
      let target = if forward then 100.0 else 0.0 in
      let duration = `For (Time_ns.Span.of_sec 0.5) in
      animate ~with_:interpolator ~after_finished:(switch_directions ()) duration target
    in
    switch_directions ()
  in
  let () = Bonsai.Edge.lifecycle ~on_activate:get_things_started graph in
  let%arr value and text_picker and interpolator_form in
  let margin = Vdom.Attr.style (Css_gen.margin_left (`Px_float value)) in
  let color =
    let v = Float.to_int (value /. 100.0 *. 255.0) in
    Vdom.Attr.style (Css_gen.color (`RGBA (Css_gen.Color.RGBA.create ~r:v ~g:v ~b:v ())))
  in
  let text = Form.value_or_default text_picker ~default:"Marquee" in
  Vdom.Node.div
    [ Form.view_as_vdom text_picker
    ; Form.view_as_vdom interpolator_form
    ; Vdom.Node.h1 ~attrs:[ margin ] [ Vdom.Node.text text ]
    ; Vdom.Node.h1 ~attrs:[ color ] [ Vdom.Node.text text ]
    ]
;;

let () = Bonsai_web.Start.start component
