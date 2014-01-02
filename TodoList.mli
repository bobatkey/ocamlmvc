type status = TODO | DONE

type todo_item =
    { label : string; status : status; }

type t =
    { items : todo_item list; pending_item : string; }

val empty : t

val add_pending_item : t -> t

val set : int -> status -> t -> t

val clear : t -> t

val update_pending : string -> t -> t
