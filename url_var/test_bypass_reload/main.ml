open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

module For_reloader = struct
  type t = Url_var of string list [@@deriving typed_variants, sexp, equal]

  let parser_for_variant : type a. a Typed_variant.t -> a Uri_parsing.Parser.t =
    let open Uri_parsing in
    function
    | Url_var -> Parser.with_prefix [] (Parser.from_remaining_path Value_parser.string)
  ;;
end

let parser =
  let parser = Uri_parsing.Parser.Variant.make (module For_reloader) in
  Uri_parsing.Versioned_parser.first_parser parser
;;

let component (local_ graph) =
  let url_var =
    Bonsai_web_ui_url_var.Typed.make
      ~navigation:`Intercept
      (module For_reloader)
      parser
      ~fallback:(fun _ _ -> failwith "Unable to parse URL")
  in
  let url = Bonsai_web_ui_url_var.value url_var in
  let time = Bonsai.Clock.approx_now ~tick_every:(Time_ns.Span.of_sec 1.) graph in
  let start_time = Bonsai.freeze time graph in
  let%arr time and start_time and url in
  let time = Time_ns.diff time start_time |> Time_ns.Span.to_string in
  {%html|
    <div>
      <div>
        <button on_click=%{fun _ -> Effect.reload_page}>
          <code>Effect.reload</code>
        </button>
        <button on_click=%{fun _ -> Bonsai_web_ui_url_var.reload_without_intercepting}>
          <code>Bonsai_web_ui_url_var.reload_without_intercepting</code>
        </button>

        <div>Seconds since last hard reload: #{time}</div>
        <div>
          URL:
          <pre>#{Sexp.to_string_hum [%sexp (url  : For_reloader.t)]}</pre>
        </div>
      </div>
    </div>
  |}
;;

let () = Bonsai_web.Start.start component ~enable_bonsai_telemetry:Enabled
