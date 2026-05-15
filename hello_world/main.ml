open! Core
open! Bonsai_web

let component _graph =
  Bonsai.return
    {%html.jsx|
      <div
        style="
          display: flex;
          align-items: center;
          justify-content: center;
          width: 100vw;
          height: 100vh;
        "
      >
        Hello, world!
      </div>
    |}
;;

let () = Bonsai_web.Start.start component
