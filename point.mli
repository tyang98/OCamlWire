module type LetterValueMap = sig
  type t
  val get : char -> int
end
