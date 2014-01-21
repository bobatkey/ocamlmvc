let _ =
  OCamlMVC.attach ~parent_id:"editor"
    (module VCR.Of
        (Editor)
        (struct
          type t = Editor.action
          let relevant = function
            | Editor.Movement _ -> false
            | _                 -> true
         end))
