(** [t] is the current state of the game*)
type t 

(** [init_state i] is a default scrabble game with [i] players*)
val init_state : int -> t


(** [board_printer s] is the representation of the scrabble game board *)
val board_printer : t -> unit
