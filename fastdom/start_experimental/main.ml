open! Core

let run () =
  Async_js.init ();
  Bonsai_web.Start.start
    Widget_fastdom_example.component
    ~use_new_experimental_implementation:true
;;

let () = run ()
