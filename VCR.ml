module type Component = sig
  include OCamlMVC.Component
  val string_of_action : action -> string
end

module Of
  (Inner  : Component)
  (Filter : sig val relevant : Inner.action -> bool end)
  : OCamlMVC.Component
  =
struct
  open OCamlMVC

  type state =
      { history : (Inner.state * Inner.action) list
      ; now     : Inner.state
      ; future  : (Inner.action * Inner.state) list
      }

  type action =
    | Inner of Inner.action
    | Undo
    | Redo

  let render_truncated_list ~f ~limit list =
    let render_item x =
      Html.(li [ span ~classes:["small"] [ text x ] ])
    in
    let rec loop i = function
      | []                   -> []
      | x::xs when i = limit -> [ render_item "..." ]
      | x::xs                -> render_item (f x)::loop (i+1) xs
    in
    loop 0 list

  let render {history;now;future} =
    let have_history = history <> [] in
    let have_future  = future <> [] in
    let open Html in
    div [ div ~classes:["row"]
            [ div ~classes:["small-centered";"small-6";"columns"]
                [ ul ~classes:["button-group";"radius"]
                    [ li [ button ~enabled:have_history ~onclick:Undo "« Undo" ]
                    ; li [ button ~enabled:have_future ~onclick:Redo "Redo »" ]
                    ]
                ]
            ]
        ; map (fun action -> Inner action) (Inner.render now)
        ; div ~classes:["row"]
            [ div ~classes:["small-6";"columns"]
                [ h6 [ text "History" ]
                ; ul ~classes:["no-bullet"]
                    (history |> render_truncated_list
                        ~f:(fun (_,act) -> Inner.string_of_action act)
                        ~limit:10)
                ]
            ; div ~classes:["small-6";"columns"]
                [ h6 [ text "Future" ]
                ; ul ~classes:["no-bullet"]
                    (future |> render_truncated_list
                        ~f:(fun (act,_) -> Inner.string_of_action act)
                        ~limit:10)
                ]
            ]
        ]

  let update = function
    | Inner action when Filter.relevant action ->
      (fun {history;now} ->
        { history = (now,action)::history
        ; now     = Inner.update action now
        ; future  = []
        })
    | Inner action ->
      (fun ({now} as t) ->
        {t with now = Inner.update action now})
    | Undo ->
      (function
        | {history=[]} as state -> state
        | {history=(prev,action)::history; now; future} ->
          {history; now=prev; future=(action,now)::future})
    | Redo ->
      (function
        | {future=[]} as state -> state
        | {future=(action,next)::future; now; history} ->
          {history=(now,action)::history; now=next; future})

  let initial =
    { history = []; now = Inner.initial; future = [] }
end
