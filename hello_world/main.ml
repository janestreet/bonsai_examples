open! Core
open! Bonsai_web

let component (local_ _graph) =
  Bonsai.return
    {%html|
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
