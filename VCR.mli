module type Component = sig
  include OCamlMVC.Component
  val string_of_action : action -> string
end

module Of (Inner : Component) : OCamlMVC.Component
