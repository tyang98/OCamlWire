open Dictionary
open Map

(** [Entry] is a module type that can be used for entries in a trie-backed
    [Dictionary]. *)
module type Entry = sig
  module Target : Map.OrderedType

  type t

  val to_list : t -> Target.t list
end

(** [Make(Entry)] is an efficient trie-backed [Dictionary] with entries of 
    type [Entry.t]. *)
module Make : functor (Entry : Entry) -> Dictionary with type entry = Entry.t