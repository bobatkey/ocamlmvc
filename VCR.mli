module type Component = sig
  include OCamlMVC.Component
  val string_of_action : action -> string
end

module type Filter = sig
  type t
  val relevant : t -> bool
end

module Of
  (Inner  : Component)
  (Filter : Filter with type t = Inner.action)
  : OCamlMVC.Component
