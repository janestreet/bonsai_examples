open! Core
open! Bonsai_web
open Bonsai.Let_syntax

module Elements = struct
  let grid children =
    {%html|
      <div
        style="
          display: grid;
          grid-template-columns: auto auto auto;
          width: min-content;
          grid-gap: 10px;
          align-items: end;

          & > * {
            white-space: nowrap;
          }
        "
      >
        *{children}
      </div>
    |}
  ;;

  let preview value =
    {%html|
      <div style="font-family: monospace; box-shadow: 0 1px 0 0 black">
        *{value}
      </div>
    |}
  ;;
end

module Fruit = struct
  type t =
    | Apple
    | Orange
    | Pear
    | Watermelon
    | Pomegranate
  [@@deriving sexp, equal, enumerate]

  let random ~but_not =
    let all =
      match but_not with
      | None -> all
      | Some but_not -> List.filter ~f:(fun x -> not (equal x but_not)) all
    in
    Random.int (List.length all) |> List.nth_exn all
  ;;
end

let component (local_ graph) =
  let unique_id, set_unique_id = Bonsai.state "unique_id" graph in
  let make_example ~description graph =
    let item1, set_item1 = Byo_browser_storage.item (module Fruit) ~unique_id graph in
    let item2, set_item2 = Byo_browser_storage.item (module Fruit) ~unique_id graph in
    let%arr item1 and set_item1 and item2 and set_item2 and unique_id and set_unique_id in
    let on_click_item1 =
      Vdom.Attr.on_click (fun _ -> set_item1 (Fruit.random ~but_not:item1))
    in
    let on_click_item2 =
      Vdom.Attr.on_click (fun _ -> set_item2 (Fruit.random ~but_not:item2))
    in
    let change_key =
      Vdom.Attr.on_click (fun _ ->
        let s = Random.ascii () |> Char.to_string in
        set_unique_id s)
    in
    let item1_str = [%message (item1 : Fruit.t option)] |> Sexp.to_string in
    let item2_str = [%message (item2 : Fruit.t option)] |> Sexp.to_string in
    let unique_id_str = [%message (unique_id : string)] |> Sexp.to_string in
    {%html|
      <div
        style="
          display: grid;
          grid-template-columns: 1fr;
          grid-gap: 10px;
          border: 1px solid black;
          padding: 10px;
          border-radius: 5px;
        "
      >
        %{description}
        <Elements.grid>
          <div>localStorage key</div>
          <Elements.preview> #{unique_id_str} </>
          <button %{change_key}>set to random key</button>
          <div>item</div>
          <Elements.preview> #{item1_str} </>
          <button %{on_click_item1}>set to random value</button>
          <div>other item reading the same key</div>
          <Elements.preview> #{item2_str} </>
          <button %{on_click_item2}>set to random value</button>
        </>
      </div>
    |}
  in
  let example1 =
    Byo_browser_storage.with_storage
      ~f:
        (make_example
           ~description:
             {%html|
               <div>
                 Wrapped with <code>Byo_browser_storage.with_storage</code>: writes should be
                 synced to both items.
               </div>
             |})
      graph
  in
  let example2 =
    make_example
      ~description:
        {%html|
          <div>
            Not wrapped with <code>Byo_browser_storage.with_storage</code>: writes should
            only affect one item. There should also be a warning in the browser console.
          </div>
        |}
      graph
  in
  let%arr example1 and example2 in
  {%html|
      <div style="max-width: min(90vw, 800px);
                  margin: auto;
                  > * {
                    margin-block: 24px;
                  }">
        <p>
          <code> Byo_browser_storage.item </code> demo. Expected behavior:
          <ul>
              <li> Values should be persisted across page refreshes </li>
              <li> If you open this same page in another tab, values should sync up between the tabs </li>
              <li> Changing the localStorage key should reset all values </li>
              <li> Same-tab syncing between items depends on whether <code>Byo_browser_storage.with_storage</code> is used. </li>
          </ul>
        </p>
        <div>
        %{example1}
        </div>
        <div>
        %{example2}
        </div>
      </div>
    |}
;;

let () = Bonsai_web.Start.start ~enable_bonsai_telemetry:Enabled component
