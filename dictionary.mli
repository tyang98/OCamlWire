module type Dictionary = sig
  type entry

  type 'a t

  val empty : 'a t

  val insert : entry -> 'a -> 'a t -> 'a t

  val get : entry -> 'a t -> 'a option

  val check : entry -> 'a t -> bool
end