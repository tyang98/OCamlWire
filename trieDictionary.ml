open Dictionary
open Map

module Make = functor (K : OrderedType) -> struct
  module Key = K

  module Children = Map.Make(Key)

  type 'a t = Node of 'a option * 'a t Children.t

  let empty = Node (None, Children.empty)

  let insert word e t = failwith "Unimplemented"

  let get word t = failwith "Unimplemented"

  let check word t = failwith "Unimplemented"
end