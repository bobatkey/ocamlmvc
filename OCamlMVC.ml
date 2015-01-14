let ( !$ ) x = Js.string x
let ( >>?= ) = Js.Opt.iter
let identity x = x
let (<.>) f g x = f (g x)

(**********************************************************************)
module StringMap = Map.Make (String)

type attributes = string StringMap.t

(**********************************************************************)
type 'action event =
  | Event : ('e #Dom.event Js.t as 'b) Dom.Event.typ
            * (Dom_html.element Js.t -> 'b -> 'action option)
    -> 'action event

type 'action events = 'action event list

(**********************************************************************)
type 'action node =
  | Map  : ('inner_action -> 'action) * 'inner_action node     -> 'action node
  | El   : string * attributes * 'action events * 'action html -> 'action node
  | Text : string                                              -> 'action node
and 'action html =
  'action node list
(*
  Idea: the function and the data are stored in the generated tree,
  and if they are both equal next time, then the generated tree
  underneath is assumed to be the same. Use physical equality.

  | Cacheable : 'a * ('a -> 'action html)                         -> 'action html
*)

(**********************************************************************)
module Html = struct
  (* FIXME:
     - Merge adjacent text nodes? (didn't do this before)
     - More efficient concatenation? just have a 'Concat' constructor?
     - Let the update function deal with the associativity?
  *)
  type 'a t = 'a html

  type 'action attribute =
    | A_Simple of string * string
    | A_Event : ('a #Dom.event Js.t as 'b) Dom.Event.typ
              * (Dom_html.element Js.t -> 'b -> 'action option)
      -> 'action attribute

  let empty = []

  let (^^) html1 html2 =
    html1 @ html2


  let of_list nodess =
    List.concat nodess

  let map f t =
    List.map (fun node -> Map (f, node)) t

  let attributes_and_handlers list =
    let attributes, handlers =
      List.fold_left
        (fun (attributes, handlers) surface_attr ->
           match surface_attr with
             | A_Simple (name, value) ->
                (StringMap.add name value attributes, handlers)
             | A_Event (typ, h) ->
                (attributes, Event (typ, h)::handlers))
        (StringMap.empty, [])
        list
    in
    (attributes, List.rev handlers)

  let elt name ?(attrs=[]) children =
    let attributes, handlers = attributes_and_handlers attrs in
    [El (name, attributes, handlers, children)]

  let elt_empty name ?attrs () =
    elt name ?attrs []

  let text text =
    [Text text]

  (* 4.1 The root element *)
  let html ?attrs = elt "html" ?attrs

  (* 4.2 Document metadata *)
  let head ?attrs = elt "head" ?attrs
  let title ?attrs = elt "title" ?attrs
  let base ?attrs = elt "base" ?attrs
  let link ?attrs = elt "link" ?attrs
  let meta ?attrs = elt "meta" ?attrs
  let style ?attrs = elt "style" ?attrs

  (* 4.3 Sections *)
  let body ?attrs = elt "body" ?attrs
  let article ?attrs = elt "article" ?attrs
  let section ?attrs = elt "section" ?attrs
  let nav ?attrs = elt "nav" ?attrs
  let aside ?attrs = elt "aside" ?attrs
  let h1 ?attrs = elt "h1" ?attrs
  let h2 ?attrs = elt "h2" ?attrs
  let h3 ?attrs = elt "h3" ?attrs
  let h4 ?attrs = elt "h4" ?attrs
  let h5 ?attrs = elt "h5" ?attrs
  let h6 ?attrs = elt "h6" ?attrs
  let header ?attrs = elt "header" ?attrs
  let footer ?attrs = elt "footer" ?attrs
  let address ?attrs = elt "address" ?attrs

  (* 4.4 Grouping content *)
  let p ?attrs = elt "p" ?attrs
  let hr ?attrs = elt_empty "hr" ?attrs
  let pre ?attrs = elt "pre" ?attrs
  let blockquote ?attrs = elt "blockquote" ?attrs
  let ol ?attrs = elt "ol" ?attrs
  let ul ?attrs = elt "ul" ?attrs
  let li ?attrs = elt "li" ?attrs
  let dl ?attrs = elt "dl" ?attrs
  let dt ?attrs = elt "dt" ?attrs
  let dd ?attrs = elt "dd" ?attrs
  let figure ?attrs = elt "figure" ?attrs
  let figcaption ?attrs = elt "figcaption" ?attrs
  let div ?attrs = elt "div" ?attrs
  let main ?attrs = elt "main" ?attrs

  (* 4.5 Text level semantics *)
  let a ?attrs = elt "a" ?attrs
  let em ?attrs = elt "em" ?attrs
  let strong ?attrs = elt "strong" ?attrs
  let small ?attrs = elt "small" ?attrs
  let s ?attrs = elt "s" ?attrs
  let cite ?attrs = elt "cite" ?attrs
  let q ?attrs = elt "q" ?attrs
  let dfn ?attrs = elt "dfn" ?attrs
  let abbr ?attrs = elt "abbr" ?attrs
  let data ?attrs = elt "data" ?attrs
  let time ?attrs = elt "time" ?attrs
  let code ?attrs = elt "code" ?attrs
  let var ?attrs = elt "var" ?attrs
  let samp ?attrs = elt "samp" ?attrs
  let kbd ?attrs = elt "kbd" ?attrs
  let sub ?attrs = elt "sub" ?attrs
  let sup ?attrs = elt "sup" ?attrs
  let i ?attrs = elt "i" ?attrs
  let b ?attrs = elt "b" ?attrs
  let u ?attrs = elt "u" ?attrs
  let mark ?attrs = elt "mark" ?attrs
  let ruby ?attrs = elt "ruby" ?attrs
  let rb ?attrs = elt "rb" ?attrs
  let rt ?attrs = elt "rt" ?attrs
  let rtc ?attrs = elt "rtc" ?attrs
  let rp ?attrs = elt "rp" ?attrs
  let bdi ?attrs = elt "bdi" ?attrs
  let bdo ?attrs = elt "bdo" ?attrs
  let span ?attrs = elt "span" ?attrs
  let br ?attrs = elt_empty "br" ?attrs
  let wbr ?attrs = elt_empty "wbr" ?attrs

  (* 4.6 Edits *)
  let ins ?attrs = elt "ins" ?attrs
  let del ?attrs = elt "del" ?attrs

  (* 4.7 Embedded content *)
  let img ?attrs = elt "img" ?attrs

  (* 4.8 Links *)

  (* 4.9 Tabular data *)
  let table ?attrs = elt "table" ?attrs
  let caption ?attrs = elt "caption" ?attrs
  let colgroup ?attrs = elt "colgroup" ?attrs
  let col ?attrs = elt "col" ?attrs
  let tbody ?attrs = elt "tbody" ?attrs
  let thead ?attrs = elt "thead" ?attrs
  let tfoot ?attrs = elt "tfoot" ?attrs
  let tr ?attrs = elt "tr" ?attrs
  let td ?attrs = elt "td" ?attrs
  let th ?attrs = elt "th" ?attrs

  (* 4.10 Forms *)
  let form ?attrs = elt "form" ?attrs
  let label ?attrs = elt "label" ?attrs
  let input ?attrs = elt "input" ?attrs
  let button ?attrs = elt "button" ?attrs
  let select ?attrs = elt "select" ?attrs
  let datalist ?attrs = elt "datalist" ?attrs
  let optgroup ?attrs = elt "optgroup" ?attrs
  let option ?attrs = elt "option" ?attrs
  let textarea ?attrs = elt "textarea" ?attrs
  let keygen ?attrs = elt "keygen" ?attrs
  let output ?attrs = elt "output" ?attrs
  let progress ?attrs = elt "progress" ?attrs
  let meter ?attrs = elt "meter" ?attrs
  let fieldset ?attrs = elt "fieldset" ?attrs
  let legend ?attrs = elt "legend" ?attrs

  (* 4.11 Scripting (which here just means 'canvas') *)

  (* Attributes *)
  module A = struct
    (* Global attributes (3.2.5) *)
    let accesskey value =
      A_Simple ("accesskey", value)
    let class_ value =
      A_Simple ("class", value)
    let contenteditable value =
      A_Simple ("contenteditable", if value then "true" else "false")
    let dir value =
      A_Simple ("dir", match value with `ltr -> "ltr" | `rtl -> "rtl" | `auto -> "auto")
    let hidden value =
      A_Simple ("hidden", if value then "true" else "false")
    let id value =
      A_Simple ("id", value)
    let lang value =
      A_Simple ("lang", value)
    let spellcheck value =
      A_Simple ("spellcheck", if value then "true" else "false")
    let style value =
      A_Simple ("style", value)
    let tabindex value =
      A_Simple ("tabindex", string_of_int value)
    let title value =
      A_Simple ("title", value)
    let translate value =
      A_Simple ("translate", if value then "yes" else "no")

    (* 'html' element attributes (4.1.1) *)
    let manifest value =
      A_Simple ("manifest", value)

    (* 'a' element attributes (4.5.1) *)
    let href value =
      A_Simple ("href", value)
    let target value =
      A_Simple ("target", value)
    let download value =
      A_Simple ("download", value)
    let rel value =
      A_Simple ("rel", value)
    let hreflang value =
      A_Simple ("hreflang", value)
    let type_ value =
      A_Simple ("type", value)

    (* For 'img' elements *)
    let src value =
      A_Simple ("src", value)

    (* For 'form' elements *)
    let accept_charset value =
      A_Simple ("accept-charset", value)
    let action value =
      A_Simple ("action", value)
    let autocomplete value =
      A_Simple ("autocomplete", value)
    let enctype value =
      A_Simple ("enctype", value)
    let http_method value =
      A_Simple ("method", value)
    let name value =
      A_Simple ("name", value)
    let novalidate value = (*FIXME: boolean?*)
      A_Simple ("novalidate", value)
    let target value =
      A_Simple ("target", value)

    (* For 'label' elements *)
    let form value =
      A_Simple ("form", value)
    let for_control value =
      A_Simple ("for", value)

    (* For 'input' elements *)
    let accept value =
      A_Simple ("accept", value)
    let alt value =
      A_Simple ("alt", value)
    (* FIXME *)
    let enabled value =
      A_Simple ("enabled", if value then "yes" else "no")
    let checked value =
      A_Simple ("checked", if value then "yes" else "no")
    let value value =
      A_Simple ("value", value)
    let placeholder value =
      A_Simple ("placeholder", value)
  end

  (* Events *)
  module E = struct
    let onkeypress f =
      A_Event
        (Dom_html.Event.keypress,
         fun node ev ->
           let char_code = Js.Optdef.get (ev##charCode) (fun () -> 0) in
           (* FIXME: and all the other attributes of ev *)
(*           let key =
             Js.to_string @@
             Js.Optdef.get (ev##keyIdentifier) (fun () -> Js.string "")
           in*)
           f (ev##keyCode) char_code)

    let onclick action =
      A_Event
        (Dom_html.Event.click,
         (* FIXME: supply the event attributes properly *)
         fun node ev -> Some action)

    let ondoubleclick action =
      A_Event
        (Dom_html.Event.dblclick,
         fun node ev -> Some action)

    let oninput f =
      A_Event
        (Dom_html.Event.input,
         fun node ev ->
           let s =
             Js.Optdef.get (* FIXME: this will return empty string if undefined *)
               ((Js.Unsafe.coerce node)##value)
               (fun _ -> Js.string "")
           in
           Some (f ~value:(Js.to_string s)))

    let onchange f =
      A_Event
        (Dom_html.Event.change,
         fun node ev ->
           let s =
             Js.Optdef.get (* FIXME: this will return empty string if undefined *)
               ((Js.Unsafe.coerce node)##value)
               (fun _ -> Js.string "")
           in
           Some (f ~value:(Js.to_string s)))
  end
end

(**********************************************************************)
type tree =
  | El_existing
    : Dom_html.element Js.t
      * string
      * attributes
      * Dom.event_listener_id list
      * trees
    -> tree
  | Text_existing : Dom.text Js.t * string -> tree
  | Dummy : tree -> tree
and trees =
  tree list

let rec node_of_tree = function
  | El_existing (node, _, _, _, _) -> (node :> Dom.node Js.t)
  | Text_existing (node, _)        -> (node :> Dom.node Js.t)
  | Dummy tree                     -> node_of_tree tree

let add_handler h node = function
  | Event (typ, func) ->
     let handler = Dom.handler (fun ev ->
         match func node ev with
           | None -> Js._true
           | Some action -> h action)
     in
     Dom.addEventListener node typ handler Js._false

let rec create : 'a. ('a -> bool Js.t) -> Dom_html.element Js.t option -> 'a node -> tree =
  fun h parent new_tree -> match new_tree with
    | Map (f, child) ->
       Dummy (create (h <.> f) parent child)

    | El (tag, attrs, events, children) ->
       let node = Dom_html.document##createElement (!$tag) in
       attrs |> StringMap.iter begin fun attr value ->
         node##setAttribute (!$attr, !$value)
       end;
       let handler_ids = List.map (add_handler h node) events in
       let children = create_nodes h (Some node) children in
       (match parent with
         | None -> () | Some parent -> Dom.appendChild parent node);
       El_existing (node, tag, attrs, handler_ids, children)

    | Text text ->
       let node = Dom_html.document##createTextNode (!$text) in
       (match parent with
         | None -> () | Some parent -> Dom.appendChild parent node);
       Text_existing (node, text)

and create_nodes : 'a. ('a -> bool Js.t) -> Dom_html.element Js.t option -> 'a html -> trees =
  fun h parent -> List.map (create h parent)

let update_attrs node existing_attrs new_attrs =
  let added_attrs =
    StringMap.fold
      (fun attr value new_attrs ->
         match StringMap.find attr new_attrs with
           | exception Not_found ->
              node##removeAttribute (!$attr);
              new_attrs
           | new_value when value = new_value ->
              StringMap.remove attr new_attrs
           | new_value ->
              node##setAttribute (!$attr, !$new_value);
              StringMap.remove attr new_attrs)
      existing_attrs
      new_attrs
  in
  added_attrs |> StringMap.iter (fun attr value ->
    node##setAttribute (!$attr, !$value))

(* FIXME: make this better: what's so special about checkboxes? *)
let set_input_props node attrs =
  Dom_html.CoerceTo.input node        >>?= fun input_node ->
  input_node##getAttribute (!$"type") >>?= fun s ->
  match Js.to_string s with
    | "checkbox" ->
       input_node##checked <- Js.bool (StringMap.mem "checked" attrs)
    | _ ->
       (try input_node##value <- !$(StringMap.find "value" attrs)
        with Not_found -> ())

let rec update_tree : 'a. ('a -> bool Js.t) -> Dom_html.element Js.t -> tree -> 'a node -> tree =
  fun h parent existing_tree new_tree ->
    match existing_tree, new_tree with
      | Dummy existing, new_tree ->
         update_tree h parent existing new_tree

      | existing, Map (f, new_tree) ->
         update_tree (h <.> f) parent existing new_tree

      | El_existing (node, tag1, attrs1, handler_ids, children1), El (tag2, attrs2, events, children2) when tag1 = tag2 ->
         List.iter Dom.removeEventListener handler_ids;
         set_input_props node attrs2;
         update_attrs node attrs1 attrs2;
         let handler_ids = List.map (add_handler h node) events in
         let children    = update_trees h node children1 children2 in
         El_existing (node, tag2, attrs2, handler_ids, children)

      | Text_existing (node, text1), Text text2 when text1 = text2 ->
         Text_existing (node, text1)

      | Text_existing (node, text1), Text text2 ->
         node##data <- !$text2;
         Text_existing (node, text2)

      | existing_tree, new_tree ->
         let tree = create h None new_tree in
         Dom.replaceChild parent
           (node_of_tree tree)
           (node_of_tree existing_tree);
         tree

and update_trees : 'a. ('a -> bool Js.t) -> Dom_html.element Js.t -> trees -> 'a node list -> trees =
  fun h parent existing_trees new_trees ->
    match existing_trees, new_trees with
      | existing_tree::existing_trees, new_tree::new_trees ->
         update_tree h parent existing_tree new_tree::
         update_trees h parent existing_trees new_trees

      | [], new_trees ->
         new_trees |> List.map (create h (Some parent))

      | old_trees, [] ->
         old_trees |> List.iter begin fun old_tree ->
           Dom.removeChild parent (node_of_tree old_tree)
         end;
         []

(**********************************************************************)
let run render update parent state =
  let current_tree = ref None in
  let rec loop state =
    let handler action = loop (update action state) in
    let html = render state in
    (match !current_tree with
      | None ->
         current_tree := Some (create_nodes handler (Some parent) html)
      | Some tree ->
         current_tree := Some (update_trees handler parent tree html));
    Js._false
  in
  loop state

(**********************************************************************)
module type Component = sig
  type state
  type action

  val render  : state -> action html
  val update  : action -> state -> state
  val initial : state
end

type component = (module Component)
type 'action component' = (module Component with type action = 'action)

let attach ~parent_id (module C : Component) =
  Dom_html.document##getElementById (!$parent_id) >>?= fun parent ->
  ignore (run C.render C.update parent C.initial)
