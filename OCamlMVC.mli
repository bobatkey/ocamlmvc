(** OCamlMVC: an MVC library for writing reactive webpages *)

type 'action html

module type Component = sig
  type state
  type action

  val render  : state -> action html
  val update  : action -> state -> state
  val initial : state
end

val attach : parent_id:string -> (module Component) -> unit

module Html : sig
  val map : ('a -> 'b) -> 'a html -> 'b html

  val div : ?classes:string list -> 'a html list -> 'a html

  val text : string -> 'a html

  val ul : ?classes:string list -> 'a html list -> 'a html

  val ol : ?classes:string list -> 'a html list -> 'a html

  val li : 'a html list -> 'a html

  val span : ?classes:string list -> 'a html list -> 'a html

  val checkbox : id:string -> onchange:'a -> state:bool -> 'a html

  val text_input :
    ?classes:string list ->
    oninput:(string -> 'a) ->
    onenter:'a -> ?placeholder:string -> string -> 'a html

  val textarea :
    ?classes:string list ->
    oninput:(string -> 'a) ->
    ?rows:int -> ?placeholder:string -> string -> 'a html

  val button :
    ?classes:string list ->
    ?enabled:bool -> onclick:'a -> string -> 'a html

  val label :
    for_id:string -> ?classes:string list -> 'a html list -> 'a html

  val hr : 'a html

  val br : 'a html

  val h1 : 'a html list -> 'a html

  val h2 : 'a html list -> 'a html

  val h3 : 'a html list -> 'a html

  val h4 : 'a html list -> 'a html

  val h5 : 'a html list -> 'a html

  val h6 : 'a html list -> 'a html

  val em : 'a html list -> 'a html

  val strong : 'a html list -> 'a html

  val code : 'a html list -> 'a html

  val pre : 'a html list -> 'a html

  val p : 'a html list -> 'a html

  val a : href:string -> 'a html list -> 'a html

  val blockquote : 'a html list -> 'a html
end
