open! Core

let () =
  Bonsai_web.Start.start Bonsai_time_example.component ~enable_bonsai_telemetry:Enabled
;;
