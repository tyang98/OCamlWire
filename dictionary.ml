module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type Dictionary = sig
  module Key : OrderedType

  type 'a t

  val insert : Key.t list -> 'a -> 'a t -> 'a t

  val get : Key.t list -> 'a t -> 'a option

  val check : Key.t list -> 'a t -> bool
end

module type DictionaryMaker =
  functor (K : OrderedType)
    -> Dictionary with module Key = K