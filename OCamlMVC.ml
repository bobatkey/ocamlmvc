let ( !$ ) = Js.string
let identity x = x
  
(**********************************************************************)
module StringMap = Map.Make (String)

type attrs = string StringMap.t

(**********************************************************************)
type tree =
  | El_existing
      of Dom_html.element Js.t
       * string
       * attrs
       * Dom.event_listener_id list
       * tree list
  | Text_existing
      of Dom.text Js.t
       * string

let node_of_tree = function
  | El_existing (node, _, _, _, _) -> (node :> Dom.node Js.t)
  | Text_existing (node, _)        -> (node :> Dom.node Js.t)

(**********************************************************************)
type 'action event =
  | Event
      : ('e #Dom.event Js.t as 'b) Dom.Event.typ
      * (Dom_html.element Js.t -> 'b -> 'action option)
      -> 'action event

type 'action events = 'action event list

(**********************************************************************)
type 'action html =
  | Map  : ('inner_action -> 'action) * 'inner_action html     -> 'action html
  | El   : string * attrs * 'action events * 'action html list -> 'action html
  | Text : string                                              -> 'action html


let add_handler h node = function
  | Event (typ, func) ->
    let handler = Dom.handler begin fun ev ->
      match func node ev with
        | None -> Js._true
        | Some action -> h action
    end
    in
    Dom.addEventListener node typ handler Js._false

let rec create : 'a. ('a -> bool Js.t) -> Dom_html.element Js.t option -> 'a html -> tree =
  fun h parent new_tree -> match new_tree with
    | Map (f, tree) ->
      create (fun action -> h (f action)) parent tree

    | El (tag, attrs, events, children) ->
      let node = Dom_html.document##createElement (!$tag) in
      attrs |> StringMap.iter begin fun attr value ->
        node##setAttribute (!$attr, !$value)
      end;
      let handler_ids = List.map (add_handler h node) events in
      let children = List.map (create h (Some node)) children in
      (match parent with
        | None -> () | Some parent -> Dom.appendChild parent node);
      El_existing (node, tag, attrs, handler_ids, children)

    | Text text ->
      let node = Dom_html.document##createTextNode (!$text) in
      (match parent with
        | None -> () | Some parent -> Dom.appendChild parent node);
      Text_existing (node, text)

let update_attrs node attrs1 attrs2 =
  let attrs2 =
    attrs2 |> StringMap.fold begin fun attr value attrs2 ->
      match try Some (StringMap.find attr attrs2) with Not_found -> None with
        | None ->
          node##removeAttribute (!$attr); attrs2
        | Some new_value when value = new_value ->
          StringMap.remove attr attrs2
        | Some new_value ->
          node##setAttribute (!$attr, !$new_value);
          StringMap.remove attr attrs2
    end attrs1
  in
  attrs2 |> StringMap.iter begin fun attr value ->
    node##setAttribute (!$attr, !$value)
  end

(* FIXME: make this better *)
let set_input_props node attrs =
  Js.Opt.iter (Dom_html.CoerceTo.input node) begin fun input_node ->
    Js.Opt.iter (input_node##getAttribute (!$"type")) begin fun s -> match Js.to_string s with
      | "checkbox" ->
        input_node##checked <- Js.bool (StringMap.mem "checked" attrs)
      | _ ->
        (try input_node##value <- !$(StringMap.find "value" attrs)
         with Not_found -> ())
    end
  end

let rec update_tree : 'a. ('a -> bool Js.t) -> Dom_html.element Js.t -> tree -> 'a html -> tree =
  fun h parent existing_tree new_tree ->
    match existing_tree, new_tree with
      | existing, Map (f, new_tree) ->
        update_tree (fun action -> h (f action)) parent existing new_tree

      | El_existing (node, tag1, attrs1, handler_ids, children1), El (tag2, attrs2, events, children2) when tag1 = tag2 ->
        handler_ids |> List.iter Dom.removeEventListener;
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

and update_trees : 'a. ('a -> bool Js.t) -> Dom_html.element Js.t -> tree list -> 'a html list -> tree list =
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
module Html = struct
  let map f tree = Map (f, tree)

  let div ?(classes=[]) children =
    El ("div",
        StringMap.singleton "class" (String.concat " " classes),
        [],
        children)

  let text text =
    Text text

  let ul ?(classes=[]) children =
    El ("ul",
        StringMap.singleton "class" (String.concat " " classes),
        [],
        children)

  let ol ?(classes=[]) children =
    El ("ol",
        StringMap.singleton "class" (String.concat " " classes),
        [],
        children)

  let li children =
    El ("li",
        StringMap.empty,
        [],
        children)

  let span ?(classes=[]) children =
    El ("span",
        StringMap.singleton "class" (String.concat " " classes),
        [],
        children)

  let checkbox ~id ~onchange:action ~state =
    El ("input",
        StringMap.(
          empty |> add "type" "checkbox"
                |> add "id" id
                |> (if state then add "checked" "yes" else identity)),
        [ Event (Dom_html.Event.change,
                 (fun _ _ -> Some action))
        ],
        [])

  let text_input ?(classes=[]) ~oninput ~onenter ?placeholder value =
    El ("input",
        StringMap.(
          empty |> add "type" "text"
                |> add "value" value
                |> add "class" (String.concat " " classes)
                |> (match placeholder with
                    | None -> identity
                    | Some placeholder -> add "placeholder" placeholder)),
        [ Event (Dom_html.Event.input,
                 (fun obj ev ->
                   let s =
                     Js.Opt.(get
                               (map
                                  (Dom_html.CoerceTo.input obj)
                                  (fun input -> input##value))
                               (fun _ -> !$"blob"))
                   in
                   Some (oninput (Js.to_string s))))
        ; Event (Dom_html.Event.keypress,
                 (fun obj ev ->
                   if ev##keyCode = Keycode.return then Some onenter else None))
        ],
        [ ])

  let textarea ?(classes=[]) ~oninput ?rows ?placeholder value =
    El ("textarea",
        StringMap.(
          empty |> add "class" (String.concat " " classes)
                |> (match placeholder with
                    | None -> identity
                    | Some placeholder -> add "placeholder" placeholder)
                |> (match rows with
                    | None -> identity
                    | Some rows -> add "rows" (string_of_int rows))),
        [ Event (Dom_html.Event.input,
                 (fun obj ev ->
                   let s =
                     Js.Opt.(get
                               (map
                                  (Dom_html.CoerceTo.textarea obj)
                                  (fun input -> input##value))
                               (fun _ -> !$"blob"))
                   in
                   Some (oninput (Js.to_string s))))
        ],
        [ Text value ])

  let button ?(classes=[]) ?(enabled=true) ~onclick text =
    let class_ = 
      String.concat " " ([ "button" ]
                         @ (if enabled then [] else ["disabled"])
                         @ classes)
    in
    El ("a",
        StringMap.(empty |> add "class" class_),
        [ Event (Dom_html.Event.click,
                 (fun _ _ -> Some onclick)) ],
        [ Text text ])

  let label ~for_id ?(classes=[]) children =
    El ("label",
        StringMap.(
          empty |> add "for" for_id
                |> add "class" (String.concat " " classes)),
        [ ],
        children)

  let hr =
    El ("hr", StringMap.empty, [], [])

  let br =
    El ("br", StringMap.empty, [], [])

  let h1 children =
    El ("h1", StringMap.empty, [], children)

  let h2 children =
    El ("h2", StringMap.empty, [], children)

  let h3 children =
    El ("h3", StringMap.empty, [], children)

  let h4 children =
    El ("h4", StringMap.empty, [], children)

  let h5 children =
    El ("h5", StringMap.empty, [], children)

  let h6 children =
    El ("h6", StringMap.empty, [], children)

  let em children =
    El ("em", StringMap.empty, [], children)

  let strong children =
    El ("strong", StringMap.empty, [], children)

  let code children =
    El ("code", StringMap.empty, [], children)

  let pre children =
    El ("pre", StringMap.empty, [], children)

  let p children =
    El ("p", StringMap.empty, [], children)

  let a ~href children =
    El ("a", StringMap.(add "href" href empty), [], children)

  let blockquote children =
    El ("blockquote", StringMap.empty, [], children)
end

(**********************************************************************)
let run render update parent state =
  let current_tree = ref None in
  let rec loop state =
    let handler action = loop (update action state) in
    let html = render state in
    (match !current_tree with
      | None ->
        current_tree := Some (create handler (Some parent) html)
      | Some tree ->
        current_tree := Some (update_tree handler parent tree html));
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

let attach ~parent_id (module C : Component) =
  let parent_opt = Dom_html.document##getElementById (!$parent_id) in
  Js.Opt.iter parent_opt begin fun parent ->
    ignore (run C.render C.update parent C.initial)
  end
