(** [t] is the current state of the game. *)
type t 

(** [init_state i] is a default scrabble game with [i] players. *)
val init_state : int -> t

(** [init_players players] is a default scrabble game with the players 
    specified in [players]*)
val init_players : Player.t list -> t

(* [execute p s] is a the new state after executing proposed move [p] 
   on the current state [s]. *)
val execute : ProposedMove.t -> t -> t option

(** [swap n s] is Some new state where the current player swaps*)
val swap : ProposedSwap.t -> t -> t option

(** [increment_turn s] is s with the next players turn as current. *)
val increment_turn : t -> t

(** [board_printer s] is the representation of the scrabble game board. *)
val board_printer : t -> unit

(** [whose_turn s] is the number of the player whose turn it is. *)
val whose_turn : t -> int

(** [get_player s i] is the player with index [i] in the list of players
    contained in the game state [s]. *)
val get_player : t -> int -> Player.t

(** [grab_tile s n pn] is the current state updated where player number [pn] 
    has taken [n] tiles from the [TileInventory]. *)
val grab_tile : t -> int -> int -> t