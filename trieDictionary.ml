open Dictionary
open Map

module type Entry = sig
  module Target : Map.OrderedType

  type t

  val to_list : t -> Target.t list
end

module Make = functor (Entry : Entry) -> struct
  module Children = Map.Make(Entry.Target)

  type entry = Entry.t

  type 'a t = Node of 'a option * 'a t Children.t

  let empty = Node (None, Children.empty)

  (** Assuming this is a helper method, may need to document. *)
  let rec insert word e trie = 
    let Node (cv, map) = trie in
    match word with
    | h::t -> begin
        match Children.find_opt h map with
        | None -> Node (cv,
                        Children.add h
                          (insert t e (Node (None, Children.empty))) map)
        | Some n -> Node (cv, Children.add h (insert t e n) map)
      end
    | [] -> Node (Some e, map)

  let insert word e trie = let word = Entry.to_list word in insert word e trie

  (** Assuming this is a helper method, may need to document. *)
  let rec get word trie =
    let Node (res, map) = trie in
    match word with
    | [] -> res
    | h::t -> match Children.find_opt h map with
      | None -> None
      | Some subtrie -> get t subtrie

  let get word trie = let word = Entry.to_list word in get word trie

  let check word trie = get word trie |> Option.is_some
end