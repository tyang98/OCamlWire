(** Representation of the Scrabble game state. *)

(** [t] is the current state of the game. *)
type t 

(** [init_state i] is a default scrabble game with [i] players. *)
val init_state : int -> t

(** [init_players players] is a default scrabble game with the players 
    specified in [players]. *)
val init_players : Player.t list -> t

(** [execute p s] is a the new state after executing proposed move [p] 
    on the current state [s]. *)
val execute : ProposedMove.t list -> t -> t option

(** [swap n s] is Some new state where the current player swaps. *)
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

(** [pass s] is Some new state where the current player passes their turn. *)
val pass : t -> t option

(** [game_over s] is whether the game represented by [s] is over. *)
val game_over : t -> bool

(** [win_game s] is whether a game has been won if a player has no
    remaining tiles left. *)
val win_game : t -> bool

(** [surrender s] is the current game but the current player has voted to 
    surrender, is None if the current player has already 
    voted to surrender. *)
val surrender : t -> t option

(** [surrender_votes s] is the number of players who have voted 
    to surrender. *)
val surrender_votes : t -> int

(** [number_of_players s] is the integer representating the number of 
    players in this current game state. *)
val number_of_players : t -> int

(** [get_player_list s] is the list of players in the game state [s]. *)
val get_player_list : t -> Player.t list
