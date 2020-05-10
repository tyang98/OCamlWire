(** [TrieDictionary] is an implementation of Dictionary using a Trie data
    structure. *)
open Dictionary
open Map

(** [Entry] is a module type that can be used for entries in a trie-backed
    [Dictionary]. *)
module type Entry = sig
  module Target : Map.OrderedType

  (** The type of an entry in the dictionary. *)
  type t

  (** [to_list e] is the list representation of entries in a 
      dictionary. *)
  val to_list : t -> Target.t list
end

(** [Make(Entry)] is an efficient trie-backed [Dictionary] with entries of 
    type [Entry.t]. *)
module Make : functor (Entry : Entry) -> Dictionary with type entry = Entry.t