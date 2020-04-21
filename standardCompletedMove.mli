(* Module representing the standard scrabble completed move *)

(** [ScrabblePoint] is a module matching [LetterValueMap] where the mappings
    are that of the original scrabble board game *)
module ScrabblePoint : CompletedMove.LetterValueMap

(** [StandardCompletedMove] is a [CompletedMove.CompletedMove] with the 
    [ScrabblePoint] value map *)
module StandardCompletedMove 
  : CompletedMove.CompletedMove with module LV = ScrabblePoint