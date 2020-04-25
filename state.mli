(** [t] is the current state of the game*)
type t 

(** [init_state i] is a default scrabble game with [i] players*)
val init_state : int -> t

(* [execute p s] is a the new state after executing proposed move [p] 
   on the current state [s] *)
val execute : ProposedMove.t -> t -> t option

(** [board_printer s] is the representation of the scrabble game board *)
val board_printer : t -> unit
