(** [LetterValueMap] is a module type for modules which map letters to 
    point values. *)
module type LetterValueMap = sig 
  val get : char -> int
end 

(** [CompletedMove] is a signature of a CompletedMove module created 
    from Make. *)
module type CompletedMove = sig
  (** [t] is the type of a completed move. *)
  type t

  (** [LV] is the LetterValueMap we use to get the values of letters*)
  module LV : LetterValueMap

  (** [score c] is the total score accomplished by this move. *)
  val score : t -> int

  (** [words c] is the list of words in this completed move. *)
  val words : t -> string list

  (** [from (word, (letter, mult), mult)] is a CompletedMove consisting of the 
      concatenation of each [word], the letter bonuses given by [letter] * 
      [mult], and the word bonuses given by [mult]. *)
  val from : (string * (char * int) list * int list) list -> t
end

(** [Make LetterVal] creates a CompletedMove from a LetterValueMap. The created
    CompletedMove module will use LetterVal to get values of letters. *)
module Make : functor (LetterVal : LetterValueMap) -> CompletedMove