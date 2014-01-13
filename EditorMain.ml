let _ =
  OCamlMVC.attach ~parent_id:"editor"
    (module VCR.Of
        (Editor)
        (struct
          let relevant = function
            | Editor.Movement _ -> false
            | _                 -> true
         end))
