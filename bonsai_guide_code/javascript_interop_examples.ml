open! Core

(* $MDX part-begin=custom_types_and_coerce *)
open Js_of_ocaml

module Window_dimensions = struct
  type window =
    < innerHeight : Js.number Js.t Js.readonly_prop
    ; innerWidth : Js.number Js.t Js.readonly_prop >

  type t =
    { height : int
    ; width : int
    }

  let number_to_int (x : Js.number Js.t) : int = Js.float_of_number x |> Int.of_float

  let get =
    (* [Js.Unsafe.coerce] is pretty much [Obj.magic]! Be very explicit about annotating
       types when using it. *)
    let window : window Js.t = Js.Unsafe.coerce Dom_html.window in
    { height = number_to_int window##.innerHeight
    ; width = number_to_int window##.innerWidth
    }
  ;;
end

(* $MDX part-end *)

let () = Fn.ignore Window_dimensions.get
