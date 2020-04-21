open Dictionary
open Map

module type Entry = sig
  module Target : Map.OrderedType

  type t

  val to_list : t -> Target.t list
end

module Make : functor (Entry : Entry) -> Dictionary with type entry = Entry.t