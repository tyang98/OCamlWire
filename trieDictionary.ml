open Dictionary
open Map

module Make = functor (K : OrderedType) -> struct
  module Key = K

  module Children = Map.Make(Key)

  type 'a t = Node of 'a option * 'a t Children.t

  let empty = Node (None, Children.empty)

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

  let rec get word trie =
    let Node (res, map) = trie in
    match word with
    | [] -> res
    | h::t -> match Children.find_opt h map with
      | None -> None
      | Some subtrie -> get t subtrie

  let check word trie = get word trie |> Option.is_some
end