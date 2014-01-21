include OCamlMVC.Component

val string_of_action : action -> string

module ActionFilter : VCR.Filter with type t = action
