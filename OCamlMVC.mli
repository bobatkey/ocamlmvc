(** An MVC library for writing reactive webpages *)

module Html : sig
  type _ t

  type _ attribute

  val empty   : 'action t
  val (^^)    : 'action t -> 'action t -> 'action t
  val of_list : 'action t list -> 'action t

  val map : ('inner_action -> 'action) -> 'inner_action t -> 'action t

  (* Text nodes *)
  val text : string -> 'a t

  (* 4.3 Sections *)
  val body : ?attrs:'action attribute list -> 'action t -> 'action t
  val article : ?attrs:'action attribute list -> 'action t -> 'action t
  val section : ?attrs:'action attribute list -> 'action t -> 'action t
  val nav : ?attrs:'action attribute list -> 'action t -> 'action t
  val aside : ?attrs:'action attribute list -> 'action t -> 'action t
  val h1 : ?attrs:'action attribute list -> 'action t -> 'action t
  val h2 : ?attrs:'action attribute list -> 'action t -> 'action t
  val h3 : ?attrs:'action attribute list -> 'action t -> 'action t
  val h4 : ?attrs:'action attribute list -> 'action t -> 'action t
  val h5 : ?attrs:'action attribute list -> 'action t -> 'action t
  val h6 : ?attrs:'action attribute list -> 'action t -> 'action t
  val header : ?attrs:'action attribute list -> 'action t -> 'action t
  val footer : ?attrs:'action attribute list -> 'action t -> 'action t
  val address : ?attrs:'action attribute list -> 'action t -> 'action t

  (* 4.4 Grouping content *)
  val p : ?attrs:'action attribute list -> 'action t -> 'action t
  val hr : ?attrs:'action attribute list -> unit -> 'action t
  val pre : ?attrs:'action attribute list -> 'action t -> 'action t
  val blockquote : ?attrs:'action attribute list -> 'action t -> 'action t
  val ol : ?attrs:'action attribute list -> 'action t -> 'action t
  val ul : ?attrs:'action attribute list -> 'action t -> 'action t
  val li : ?attrs:'action attribute list -> 'action t -> 'action t
  val dl : ?attrs:'action attribute list -> 'action t -> 'action t
  val dt : ?attrs:'action attribute list -> 'action t -> 'action t
  val dd : ?attrs:'action attribute list -> 'action t -> 'action t
  val figure : ?attrs:'action attribute list -> 'action t -> 'action t
  val figcaption : ?attrs:'action attribute list -> 'action t -> 'action t
  val div : ?attrs:'action attribute list -> 'action t -> 'action t
  val main : ?attrs:'action attribute list -> 'action t -> 'action t

  (* 4.5 Text level semantics *)
  val a : ?attrs:'action attribute list -> 'action t -> 'action t
  val em : ?attrs:'action attribute list -> 'action t -> 'action t
  val strong : ?attrs:'action attribute list -> 'action t -> 'action t
  val small : ?attrs:'action attribute list -> 'action t -> 'action t
  val s : ?attrs:'action attribute list -> 'action t -> 'action t
  val cite : ?attrs:'action attribute list -> 'action t -> 'action t
  val q : ?attrs:'action attribute list -> 'action t -> 'action t
  val dfn : ?attrs:'action attribute list -> 'action t -> 'action t
  val abbr : ?attrs:'action attribute list -> 'action t -> 'action t
  val data : ?attrs:'action attribute list -> 'action t -> 'action t
  val time : ?attrs:'action attribute list -> 'action t -> 'action t
  val code : ?attrs:'action attribute list -> 'action t -> 'action t
  val var : ?attrs:'action attribute list -> 'action t -> 'action t
  val samp : ?attrs:'action attribute list -> 'action t -> 'action t
  val kbd : ?attrs:'action attribute list -> 'action t -> 'action t
  val sub : ?attrs:'action attribute list -> 'action t -> 'action t
  val sup : ?attrs:'action attribute list -> 'action t -> 'action t
  val i : ?attrs:'action attribute list -> 'action t -> 'action t
  val b : ?attrs:'action attribute list -> 'action t -> 'action t
  val u : ?attrs:'action attribute list -> 'action t -> 'action t
  val mark : ?attrs:'action attribute list -> 'action t -> 'action t
  val ruby : ?attrs:'action attribute list -> 'action t -> 'action t
  val rb : ?attrs:'action attribute list -> 'action t -> 'action t
  val rt : ?attrs:'action attribute list -> 'action t -> 'action t
  val rtc : ?attrs:'action attribute list -> 'action t -> 'action t
  val rp : ?attrs:'action attribute list -> 'action t -> 'action t
  val bdi : ?attrs:'action attribute list -> 'action t -> 'action t
  val bdo : ?attrs:'action attribute list -> 'action t -> 'action t
  val span : ?attrs:'action attribute list -> 'action t -> 'action t
  val br : ?attrs:'action attribute list -> unit -> 'action t
  val wbr : ?attrs:'action attribute list -> unit -> 'action t

  (* 4.6 Edits *)
  val ins : ?attrs:'action attribute list -> 'action t -> 'action t
  val del : ?attrs:'action attribute list -> 'action t -> 'action t

  (* 4.7 Embedded content *)
  val img : ?attrs:'action attribute list -> 'action t -> 'action t
  (* FIXME... *)

  (* 4.9 Tabular data *)
  val table : ?attrs:'action attribute list -> 'action t -> 'action t
  val caption : ?attrs:'action attribute list -> 'action t -> 'action t
  val colgroup : ?attrs:'action attribute list -> 'action t -> 'action t
  val col : ?attrs:'action attribute list -> 'action t -> 'action t
  val tbody : ?attrs:'action attribute list -> 'action t -> 'action t
  val thead : ?attrs:'action attribute list -> 'action t -> 'action t
  val tfoot : ?attrs:'action attribute list -> 'action t -> 'action t
  val tr : ?attrs:'action attribute list -> 'action t -> 'action t
  val td : ?attrs:'action attribute list -> 'action t -> 'action t
  val th : ?attrs:'action attribute list -> 'action t -> 'action t

  (* 4.10 Forms *)
  val form : ?attrs:'action attribute list -> 'action t -> 'action t
  val label : ?attrs:'action attribute list -> 'action t -> 'action t
  val input : ?attrs:'action attribute list -> 'action t -> 'action t
  val button : ?attrs:'action attribute list -> 'action t -> 'action t

  (* 4.11 Scripting *)

  (* Attributes *)
  module A : sig
    (* Global attributes (3.2.5) *)
    val accesskey : string -> _ attribute
    val class_ : string -> _ attribute
    val contenteditable : bool -> _ attribute
    val dir : [`ltr | `rtl | `auto] -> _ attribute
    val hidden : bool -> _ attribute
    val id : string -> _ attribute
    val lang : string -> _ attribute
    val spellcheck : bool -> _ attribute
    val style : string -> _ attribute
    val tabindex : int -> _ attribute
    val title : string -> _ attribute
    val translate : bool -> _ attribute

    (* 'a' attributes *)
    val href : string -> _ attribute
    val target : string -> _ attribute
    val download : string -> _ attribute
    val rel : string -> _ attribute
    val hreflang : string -> _ attribute
    val type_ : string -> _ attribute

    (* 'img' attributes *)
    val src : string -> _ attribute

    (* 'form' attributes *)
    val accept_charset : string -> _ attribute
    val action : string -> _ attribute
    val autocomplete : string -> _ attribute
    val enctype : string -> _ attribute
    val http_method : string -> _ attribute
    val name : string -> _ attribute
    val novalidate : string -> _ attribute
    val target : string -> _ attribute

    (* 'label' attributes *)
    val form : string -> _ attribute
    val for_control : string -> _ attribute

    (* 'input' attributes *)
    val enabled : bool -> _ attribute
    val checked : bool -> _ attribute
    val value   : string -> _ attribute
    val placeholder : string -> _ attribute
  end

  module E : sig
    val onkeypress    : (int -> int -> 'action option) -> 'action attribute
    val onclick       : 'action -> 'action attribute
    val ondoubleclick : 'action -> 'action attribute
    val oninput       : (value:string -> 'action) -> 'action attribute
    val onchange      : (value:string -> 'action) -> 'action attribute
  end
end

module type Component = sig
  type state
  type action

  val render  : state -> action Html.t
  val update  : action -> state -> state
  val initial : state
end

type component = (module Component)
type 'action component' = (module Component with type action = 'action)

val attach : parent_id:string -> component -> unit
