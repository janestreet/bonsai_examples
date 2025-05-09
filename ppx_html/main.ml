open! Core
open! Bonsai_web
open! Bonsai.Let_syntax
module Gallery = Bonsai_web_ui_gallery

let centered =
  [%css
    {|
      display: flex;
      justify-content: center;
      align-items: center;
      flex-direction: column;
    |}]
;;

module Basic = struct
  let name = "PPX HTML"
  let description = {|This lets you write Vdom.Node.t's with HTML syntax!|}

  let image_url =
    "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e1/Cattle_tyrant_%28Machetornis_rixosa%29_on_Capybara.jpg/1920px-Cattle_tyrant_%28Machetornis_rixosa%29_on_Capybara.jpg"
  ;;

  let view =
    let vdom, demo =
      [%demo
        {%html|
          <div %{centered}>
            <p>Capybaras are the worlds largest living rodent.</p>
            <br />
            <img style="width: 50%" src=%{image_url} />
          </div>
        |}]
    in
    fun (local_ _graph) -> Bonsai.return (vdom, demo)
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module With_tailwind = struct
  let name = "PPX HTML + Tailwind"

  let description =
    {xxx|To use ppx_tailwind more ergonomically, we have included a short-hand notation that lets you specify which tailwind classes to use without you needing to write [%tailwind {||}]. |xxx}
  ;;

  let view =
    let vdom, demo = [%demo [%html {|<div tailwind="w-16 h-16 bg-amber-400"></div>|}]] in
    fun (local_ _graph) -> Bonsai.return (vdom, demo)
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module With_ppx_css = struct
  let name = "PPX HTML + PPX CSS"

  let description =
    {xxx|Using a "style" tag will call {%css||}. This lets you use ppx_css interpolation from ppx_html.|xxx}
  ;;

  let view =
    let vdom, demo =
      [%demo
        let color = `Name "tomato" in
        {%html|
          <div
            style="width: 2rem; height: 2rem; background-color: %{color#Css_gen.Color}"
          ></div>
        |}]
    in
    fun (local_ _graph) -> Bonsai.return (vdom, demo)
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Svg_example = struct
  let name = "HTML with Virtual_dom_svg"

  let description =
    {|By default, ppx_html will look for node and attribute functions inside the "Virtual_dom.Vdom." namespace, but this can be overridden.  For example, if you want to use Virtual_dom_svg, you could write this:|}
  ;;

  let view =
    let vdom, demo =
      [%demo
        [%html.Virtual_dom_svg
          {|
            <svg width=%{100.0} height=%{100.0}>
              <circle
                cx=%{50.0}
                cy=%{50.0}
                r=%{40.0}
                stroke=%{`Name "rebeccapurple"}
                stroke_width=%{4.0}
                fill=%{`Name "tomato"}
              ></circle>
            </svg>
          |}]]
    in
    fun (local_ _graph) -> Bonsai.return (vdom, demo)
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Plain_html = struct
  let name = "Plain HTML"

  let description =
    {xxx|You can write HTML inside of [%html {||}] or {%html||} . <f></f> will call Vdom.Node.f|xxx}
  ;;

  let view =
    let vdom, demo = [%demo {%html|<h1>Hello World!</h1>|}] in
    fun (local_ _graph) -> Bonsai.return (vdom, demo)
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Html_with_vdom_interpolation = struct
  let name = "Interpolating a Vdom node"

  let description =
    {xxx|Similar to ppx_string, you can embed OCaml expressions inside of the HTML string using %{ EXPR} syntax. Depending on whether in the HTML you put the %{} different types are expected. In these examples, a Vdom.Node.t is expected.|xxx}
  ;;

  let view =
    let vdom, demo =
      [%demo
        let body =
          Vdom.Node.div
            [ Vdom.Node.text "Capybara's are the world's largest living rodent" ]
        in
        {%html|
          <div>
            <h1>Hello Capybara!</h1>
            %{body}
          </div>
        |}]
    in
    fun (local_ _graph) -> Bonsai.return (vdom, demo)
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Html_no_string_interpolation = struct
  let name = "Interpolating a string"

  let description =
    {|Interpolating stringable types can be done similar to ppx_string. %{EXPR#Foo} will call [Vdom.Node.text (Foo.to_string EXPR)]. |}
  ;;

  let view =
    let vdom, demo =
      [%demo
        let name = "Capybara" in
        let age = 2 in
        {%html|
          <div>
            <h1>Hello #{name}! Your age is %{age#Int}.</h1>
          </div>
        |}]
    in
    fun (local_ _graph) -> Bonsai.return (vdom, demo)
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Html_with_attributes = struct
  let name = "HTML with attributes"

  let description =
    {|You can similarly use attributes. This will call attributes from Vdom.Attr.ATTRIBUTE_NAME|}
  ;;

  let view =
    let vdom, demo =
      [%demo
        let image = "Capybara_mother_with_pups.jpg" in
        {%html|
          <img
            src="https://upload.wikimedia.org/wikipedia/commons/1/1b/%{image}"
            style="width: 50%"
            tabindex=%{1}
          />
        |}]
    in
    fun (local_ _graph) -> Bonsai.return (vdom, demo)
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Attribute_value_interpolation = struct
  let name = "HTML attribute value interpolation"

  let description =
    {|If you use interpolation on an attribute’s value, you can supply it as a parameter. The type that is expected in this interpolation depends on the type of the attribute function that you are using. “Vdom.Attr.on_click” expects a function, but `src` expects a string.|}
  ;;

  let view =
    let vdom, demo =
      [%demo
        {%html|
          <div
            on_click=%{fun _ -> Effect.alert "You clicked me!"}
            style="background-color: tomato; padding: 1rem; cursor: pointer"
          >
            Click me!
          </div>
        |}]
    in
    fun (local_ _graph) -> Bonsai.return (vdom, demo)
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Attribute_interpolation = struct
  let name = {|HTML attribute interpolation|}
  let description = {|You can also interpolate an entire Attr.t:|}

  let view =
    let vdom, demo =
      [%demo
        let tomato_attr ~size =
          Vdom.Attr.many
            [ {%css|
                background-color: tomato;
                width: %{`Rem size#Css_gen.Length};
                height: %{`Rem size#Css_gen.Length};
                cursor: pointer;
              |}
            ; Vdom.Attr.on_click (fun _ -> Effect.alert "You clicked me!")
            ]
        in
        {%html|
          <div>
            <div %{tomato_attr ~size:1.0}></div>
            <div %{tomato_attr ~size:2.0}></div>
            <div %{tomato_attr ~size:3.0}></div>
          </div>
        |}]
    in
    fun (local_ _graph) -> Bonsai.return (vdom, demo)
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Interpolating_the_view_function = struct
  let name = "Tag Intepolation"

  let description =
    {|For one-off custom elements, you can replace the tag name of an element with an interpolation.  The OCaml expression inside the interpolation should have the type ?attrs:Vdom.Attr.t list -> Vdom.Node.t list -> Vdom.Node.t|}
  ;;

  let view =
    let vdom, demo =
      [%demo
        let box =
          {%css|
            background-color: tomato;
            width: 2rem;
            height: 2rem;
          |}
        in
        {%html|
          <%{View.hbox ~gap:(`Rem 1.0)}>
            <div %{box}></div>
            <div %{box}></div>
            <div %{box}></div>
            <div %{box}></div>
          </>
        |}]
    in
    fun (local_ _graph) -> Bonsai.return (vdom, demo)
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Shorter_tag_interpolation_and_ocaml_args = struct
  let name = "Calling OCaml functions and passing arguments"

  let description =
    {|This is syntax sugar that makes it easier to use OCaml functions as tags.|}
  ;;

  let view =
    let vdom, demo =
      [%demo
        let module Box = struct
          module Wrapper = struct
            let component children = Vdom.Node.div children
          end

          let component ?(color = "tomato") () =
            {%html|
              <div
                style="background-color: %{`Name color#Css_gen.Color}; width: 2rem; height: 2rem"
              ></div>
            |}
          ;;
        end
        in
        {%html|
          <View.hbox ~gap:%{`Rem 1.0}>
            <Box.Wrapper.component>
              <Box.component />
            </Box.Wrapper.component>
            <Box.Wrapper.component>
              <Box.component />
            </>
            <Box.component ~color:%{"rebeccapurple"} />
            <Box.component />
          </>
        |}]
    in
    fun (local_ _graph) -> Bonsai.return (vdom, demo)
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Interpolating_an_option = struct
  let name = "Option Interpolation"

  let description =
    {|For optional vdom nodes and attributes, you can use ?{}, similar to OCaml optional arguments.|}
  ;;

  let view =
    let vdom, demo =
      [%demo
        let maybe_tomato =
          Some
            {%css|
              background-color: tomato;
              height: 2rem;
              width: 2rem;
            |}
        in
        let maybe_greeting = Some {%html|hi|} in
        {%html|
          <div ?{maybe_tomato : Vdom.Attr.t option} %{centered}>
            ?{maybe_greeting : Vdom.Node.t option}
          </div>
        |}]
    in
    fun (local_ _graph) -> Bonsai.return (vdom, demo)
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

module Interpolating_a_list = struct
  let name = "List Interpolation"

  let description =
    {|For lists of vdom nodes and attributes, you can use *{}, similar to python's list unpacking.|}
  ;;

  let view =
    let vdom, demo =
      [%demo
        let capybaras =
          [ "Pebble"; "Cocoa"; "Squeak"; "Chomp" ]
          |> List.map ~f:(fun capy -> {%html|<li>%{capy#String}</li>|})
        in
        {%html|
          <div>
            <h1>Capybara List:</h1>
            <ul>
              *{capybaras : Vdom.Node.t list}
            </ul>
          </div>
        |}]
    in
    fun (local_ _graph) -> Bonsai.return (vdom, demo)
  ;;

  let selector = None
  let filter_attrs = Some (fun k _ -> not (String.is_prefix k ~prefix:"style"))
end

let component (local_ graph) =
  let%sub theme, theme_picker = Gallery.Theme_picker.component ~default:Kado () graph in
  View.Theme.set_for_app
    theme
    (Gallery.make_sections
       ~theme_picker
       [ ( "PPX HTML"
         , {|[ppx_html] allows you to write your virtual dom nodes with HTML syntax, spiritually similar to JSX.|}
         , [ Gallery.make_demo (module Basic)
           ; Gallery.make_demo (module Plain_html)
           ; Gallery.make_demo (module Html_with_vdom_interpolation)
           ; Gallery.make_demo (module Html_no_string_interpolation)
           ; Gallery.make_demo (module Html_with_attributes)
           ; Gallery.make_demo (module Attribute_value_interpolation)
           ; Gallery.make_demo (module Attribute_interpolation)
           ; Gallery.make_demo (module With_ppx_css)
           ; Gallery.make_demo (module Svg_example)
           ; Gallery.make_demo (module With_tailwind)
           ; Gallery.make_demo (module Interpolating_the_view_function)
           ; Gallery.make_demo (module Shorter_tag_interpolation_and_ocaml_args)
           ; Gallery.make_demo (module Interpolating_an_option)
           ; Gallery.make_demo (module Interpolating_a_list)
           ] )
       ])
    graph
;;

let () = Bonsai_web.Start.start component ~enable_bonsai_telemetry:Enabled
