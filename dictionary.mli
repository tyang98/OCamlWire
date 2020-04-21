(** A type that can be compared to be put in an order. *)
module type OrderedType = sig
  type t
  (** A total ordering function. This is a two-argument
      function [f] such that [f e1 e2] is zero if the elements [e1] and [e2] are
      equal, [f e1 e2] is strictly negative if [e1] is smaller than [e2], and
      [f e1 e2] is strictly positive if e1 is greater than e2. *)
  val compare : t -> t -> int
end

module type Dictionary = sig
  module Key : OrderedType

  type 'a t

  val empty : 'a t

  val insert : Key.t list -> 'a -> 'a t -> 'a t

  val get : Key.t list -> 'a t -> 'a option

  val check : Key.t list -> 'a t -> bool
end

module type DictionaryMaker =
  functor (K : OrderedType)
    -> Dictionary with module Key = K