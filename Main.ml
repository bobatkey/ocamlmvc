let _ =
  OCamlMVC.attach ~parent_id:"demo"
    (module VCR.Of (TodoListComponent))
