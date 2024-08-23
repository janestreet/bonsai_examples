open! Core
open! Bonsai_web

module Css =
  [%css
  stylesheet
    {|
      @font-face {
        font-family: "FiraCode";
        src: url(./font.ttf) format("truetype");
      }

      .firacode {
        font-family: "FiraCode";
      }
      |}]

let component (local_ _graph) =
  Bonsai.return
    (Vdom.Node.div
       ~attrs:[ Css.firacode ]
       [ Vdom.Node.text "text with some ligatures -> ==> >>=" ])
;;

let () = Bonsai_web.Start.start component
