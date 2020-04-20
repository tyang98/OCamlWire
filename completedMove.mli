module type LetterValueMap = sig 
  type t

  val get : char -> int
end 



module CompletedMove : functor (LetterValue : LetterValueMap) -> sig
  type t

  val score : t -> int
  val words : t -> string list
end