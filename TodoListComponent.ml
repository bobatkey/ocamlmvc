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
  let checkbox ~id ~onchange ~state =
    let attrs =
      [ A.type_ "checkbox"; A.id id; E.onchange (fun ~value -> onchange) ]
    in
    let attrs =
      if state then A.checked true::attrs else attrs
    in
    input ~attrs Html.empty
  in
  match status with
    | DONE ->
       checkbox ~id ~onchange:(Set (idx, TODO)) ~state:true
       ^^
       label ~attrs:[A.for_control id] (span ~attrs:[A.class_ "done"] (text l))
    | TODO ->
       checkbox ~id ~onchange:(Set (idx, DONE)) ~state:false
       ^^
       label ~attrs:[A.for_control id] (text l)

let render_items = let open Html in function
  | [] ->
    em (text "Items will appear here")
  | items ->
    ul ~attrs:[A.class_ "no-bullet"] begin
      of_list (List.mapi (fun i -> li <.> render_item i) items)
    end

let render {TodoList.items;pending_item} =
  let open Html in
  let div ~classes html =
    div ~attrs:[A.class_ (String.concat " " classes)] html
  in
  let text_input ~classes ~onenter ~oninput ~placeholder value =
    input
      ~attrs:[ A.class_ (String.concat " " classes)
             ; A.type_ "text"
             ; A.value value
             ; A.placeholder placeholder
             ; E.oninput oninput
             ; E.onkeypress (fun key_code char_code ->
                 if key_code = Keycode.return
                 then Some onenter else None)
             ]
      empty
  and button ~classes ~enabled ~onclick label =
    let classes =
      ["button"]
      @ (if enabled then [] else ["disabled"])
      @ classes
    in
    button ~attrs:[ A.class_ (String.concat " " classes)
                  ; E.onclick onclick
                  ]
      (text label)
  in
  div ~classes:["panel";"radius"] begin
    div ~classes:["row";"collapse"] begin
      div ~classes:["small-8";"columns"] begin
        text_input
          ~classes:["radius"]
          ~onenter:Add
          ~oninput:(fun ~value -> UpdatePending value)
          ~placeholder:"Enter new item here"
          pending_item
      end
      ^^
      div ~classes:["small-2";"columns"] begin
        button
          ~classes:["postfix"]
          ~enabled:(pending_item <> "")
          ~onclick:Add
          "Add"
      end
      ^^
      div ~classes:["small-2";"columns"] begin
        button
          ~classes:["radius";"postfix";"alert"]
          ~enabled:(List.exists (fun item -> item.TodoList.status = TodoList.DONE) items)
          ~onclick:Clear
            "Clear"
      end
      ^^
      render_items items
      ^^
      hr ()
      ^^
      div ~classes:["row"] begin
        div ~classes:["small-12";"columns"] begin
          let num_items = List.length items in
          let num_done_items =
            List.length (List.filter (fun i -> i.TodoList.status = TodoList.DONE) items)
          in
          text (sprintf "%d item%s with %d completed"
                  num_items
                  (if num_items = 1 then "" else "s")
                  num_done_items)
        end
      end
    end
  end

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
