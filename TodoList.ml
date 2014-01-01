type status = TODO | DONE

type todo_item =
    { label : string; status : status }

type t =
    { items : todo_item list; pending_item : string }

let empty =
  { items = [ ]; pending_item = "" }

(* FIXME: use some kind of random-access list thing: Phil Bagwell's VLists? *)
let modify i f l =
  let rec loop i l = match i, l with
    | _, []    -> []
    | 0, x::xs -> f x::xs
    | n, x::xs -> x::loop (i-1) xs
  in
  loop i l

let add_pending_item ({items;pending_item} as todo_list) =
  if pending_item = "" then
    todo_list
  else
    let item = {label = pending_item; status = TODO} in
    { items = item::items; pending_item = "" }

let set idx status ({items} as todo_list) =
  { todo_list
    with items = modify idx (fun item -> {item with status}) items}

let clear ({items} as todo_list) =
  { todo_list
    with items = List.filter (fun item -> item.status = TODO) items}

let update_pending new_pending_item todo_list =
  { todo_list with pending_item = new_pending_item }
