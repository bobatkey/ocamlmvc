open OCamlMVC
open Printf

let (<.>) f g x = f (g x)

(**********************************************************************)
type state = TodoList.t

type action =
  | Add
  | Set of int * TodoList.status
  | Clear
  | UpdatePending of string

let string_of_action = function
  | Add -> "Add"
  | Set (i, TodoList.TODO) -> sprintf "Set (%d, TODO)" i
  | Set (i, TodoList.DONE) -> sprintf "Set (%d, DONE)" i
  | Clear -> "Clear"
  | UpdatePending s -> sprintf "UpdatePending %S" s

(**********************************************************************)
let render_item idx {TodoList.label=l;status} =
  let id = sprintf "checkbox%d" idx in
  let open Html in
  let open TodoList in
  match status with
    | DONE ->
      [ checkbox ~id ~onchange:(Set (idx, TODO)) ~state:true
      ; label ~for_id:id [ span ~classes:["done"] [ text l ] ]
      ]
    | TODO ->
      [ checkbox ~id ~onchange:(Set (idx, DONE)) ~state:false
      ; label ~for_id:id [ text l ]
      ]

let render_items = let open Html in function
  | [] ->
    em [ text "Items will appear here" ]
  | items ->
    ul ~classes:["no-bullet"]
      (List.mapi (fun i -> li <.> render_item i) items)

let render {TodoList.items;pending_item} =
  let open Html in
  div ~classes:["panel";"radius"]
    [ div ~classes:["row";"collapse"]
        [ div ~classes:["small-8";"columns"]
            [ text_input
                ~classes:["radius"]
                ~onenter:Add
                ~oninput:(fun s -> UpdatePending s)
                ~placeholder:"Enter new item here"
                pending_item
            ]
        ; div ~classes:["small-2";"columns"]
            [ button
                ~classes:["postfix"]
                ~enabled:(pending_item <> "")
                ~onclick:Add
                "Add"
            ]
        ; div ~classes:["small-2";"columns"]
            [ button
                ~classes:["radius";"postfix";"alert"]
                ~enabled:(List.exists (fun item -> item.TodoList.status = TodoList.DONE) items)
                ~onclick:Clear
                "Clear"
            ]
        ]
    ; render_items items
    ; hr
    ; div ~classes:["row"]
        [ div ~classes:["small-12";"columns"]
            [ let num_items = List.length items in
              let num_done_items =
                List.length (List.filter (fun i -> i.TodoList.status = TodoList.DONE) items)
              in
              text (sprintf "%d item%s with %d completed"
                      num_items
                      (if num_items = 1 then "" else "s")
                      num_done_items)
            ]
        ]
    ]

let update = let open TodoList in function
  | Add                -> add_pending_item
  | Set (idx, status)  -> set idx status
  | Clear              -> clear
  | UpdatePending text -> update_pending text

let initial =
  TodoList.empty

module ActionFilter = struct
  type t = action
  let relevant = function
    | UpdatePending _ -> false
    | _               -> true
end
