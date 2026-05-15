open! Core

let run () =
  Async_js.init ();
  Bonsai_web.Start.start Widget_fastdom_example.component
;;

let () = run ()
