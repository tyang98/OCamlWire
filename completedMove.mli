module type LetterValueMap = sig 
  val get : char -> int
end 

module type CompletedMove = sig
  type t

  module LV : LetterValueMap

  val score : t -> int
  val words : t -> string list
end

module Make : functor (LetterVal : LetterValueMap) -> CompletedMove