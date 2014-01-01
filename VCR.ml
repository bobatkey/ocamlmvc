open OCamlMVC

module Of (Inner : Component) : Component = struct
  type state =
      { history : Inner.state list
      ; now     : Inner.state
      ; future  : Inner.state list
      }

  type action =
    | Inner of Inner.action
    | Undo
    | Redo

  let render {history;now;future} =
    let have_history = history <> [] in
    let have_future  = future <> [] in
    let open Html in
    let inner_view = map (fun action -> Inner action) (Inner.render now) in
    div [ div ~classes:["row"]
            [ div ~classes:["small-centered";"small-8";"columns"]
                [ ul ~classes:["button-group";"radius"]
                    [ li [ button ~enabled:have_history ~onclick:Undo "« Undo" ]
                    ; li [ button ~enabled:have_future ~onclick:Redo "Redo »" ]
                    ]
                ]
            ]
        ; inner_view
        ]

  let update = function
    | Inner action ->
      (fun {history;now} ->
        { history = now::history
        ; now     = Inner.update action now
        ; future  = []
        })
    | Undo ->
      (function
        | {history=[]} as state -> state
        | {history=prev::history; now; future} ->
          {history; now=prev; future=now::future})
    | Redo ->
      (function
        | {future=[]} as state -> state
        | {future=next::future; now; history} ->
          {history=now::history; now=next; future=future})

  let initial =
    { history = []; now = Inner.initial; future = [] }
end
