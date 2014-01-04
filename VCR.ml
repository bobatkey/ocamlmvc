module type Component = sig
  include OCamlMVC.Component
  val string_of_action : action -> string
end

module Of (Inner : Component) : OCamlMVC.Component = struct
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

  let render {history;now;future} =
    let have_history = history <> [] in
    let have_future  = future <> [] in
    let open Html in
    div [ div ~classes:["row"]
            [ div ~classes:["small-centered";"small-8";"columns"]
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
                    (List.map (fun (_, act) -> li [ span ~classes:["small"] [ text (Inner.string_of_action act) ] ]) history)
                ]
            ; div ~classes:["small-6";"columns"]
                [ h6 [ text "Future" ]
                ; ul ~classes:["no-bullet"]
                    (List.map (fun (act, _) -> li [ span ~classes:["small"] [ text (Inner.string_of_action act) ] ]) future)
                ]
            ]
        ]

  let update = function
    | Inner action ->
      (fun {history;now} ->
        { history = (now,action)::history
        ; now     = Inner.update action now
        ; future  = []
        })
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
