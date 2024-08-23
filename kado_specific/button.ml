open! Core
open! Bonsai_web
open Bonsai.Let_syntax
module Autogen = Bonsai_web_ui_auto_generated
module Form = Bonsai_web_ui_form.With_automatic_view

module Parameters = struct
  module Disabled = Bool

  module Thinking = struct
    include Bool

    let to_attr = function
      | true -> Kado.Unstable.Buttons.thinking
      | false -> Vdom.Attr.empty
    ;;
  end

  module Pressed = struct
    include Bool

    let to_attr = function
      | true -> Kado.Unstable.Buttons.pressed
      | false -> Vdom.Attr.empty
    ;;
  end

  module Group = struct
    type t =
      | Horizontal
      | Vertical
    [@@deriving sexp_grammar, sexp, enumerate, equal]

    let to_attr = function
      | Horizontal -> Kado.Unstable.Buttons.horizontal_group
      | Vertical -> Kado.Unstable.Buttons.vertical_group
    ;;
  end

  module Size = struct
    type t =
      | Normal
      | Small
    [@@deriving sexp_grammar, sexp, enumerate, equal]

    let to_attr = function
      | Normal -> Vdom.Attr.empty
      | Small -> Kado.Unstable.Buttons.small
    ;;
  end

  type t =
    { disabled : Disabled.t
    ; thinking : Thinking.t
    ; group : Group.t
    ; size : Size.t
    ; pressed : Pressed.t
    }
  [@@deriving sexp, sexp_grammar, enumerate, equal]

  let default =
    { disabled = false
    ; thinking = false
    ; group = Vertical
    ; size = Normal
    ; pressed = false
    }
  ;;
end

let form_store =
  Bonsai_web.Persistent_var.create
    (module Parameters)
    `Local_storage
    ~unique_id:"kado_specific-form"
    ~default:Parameters.default
;;

let component (local_ graph) =
  let theme = View.Theme.current graph in
  let form =
    Form.Dynamic.with_default
      (Persistent_var.value form_store)
      (Autogen.form (module Parameters) graph ~allow_updates_when_focused:`Never)
      graph
  in
  let%sub () =
    Bonsai_extra.mirror
      ()
      ~sexp_of_model:[%sexp_of: Parameters.t]
      ~equal:[%equal: Parameters.t]
      ~store_set:(Bonsai.return (Persistent_var.effect form_store))
      ~store_value:(Persistent_var.value form_store)
      ~interactive_value:
        (let%map form in
         Form.value_or_default form ~default:Parameters.default)
      ~interactive_set:
        (let%map form in
         Form.set form)
      graph
  in
  let%arr theme and form in
  let button_group { Parameters.disabled; thinking; pressed; group; size } =
    let attr =
      Vdom.Attr.many
        [ Parameters.Size.to_attr size
        ; Parameters.Thinking.to_attr thinking
        ; Parameters.Pressed.to_attr pressed
        ]
    in
    Vdom.Node.div
      ~attrs:[ Parameters.Group.to_attr group ]
      [ View.button ~attrs:[ attr ] theme ~disabled ~on_click:Ui_effect.Ignore "hello"
      ; View.button
          ~attrs:[ attr ]
          theme
          ~disabled
          ~intent:Info
          ~on_click:Ui_effect.Ignore
          "world"
      ; View.button
          ~attrs:[ attr ]
          theme
          ~disabled
          ~intent:Success
          ~on_click:Ui_effect.Ignore
          "ok"
      ; View.button
          ~attrs:[ attr ]
          theme
          ~disabled
          ~intent:Warning
          ~on_click:Ui_effect.Ignore
          "lets"
      ; View.button
          ~attrs:[ attr ]
          theme
          ~disabled
          ~intent:Error
          ~on_click:Ui_effect.Ignore
          "go"
      ]
  in
  [ Vdom.Node.div
      ~attrs:[ Vdom.Attr.style (Css_gen.create ~field:"max-width" ~value:"fit-content") ]
      [ Form.view_as_vdom form ]
  ; button_group (Form.value_or_default form ~default:Parameters.default)
  ]
;;
