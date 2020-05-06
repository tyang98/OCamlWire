(** [t] is the type representing a player. *)
type t 

(** [tile] represents the type of tiles contained in a players inventory. *)
type tile = TileInventory.tile

module CM = StandardCompletedMove.StandardCompletedMove

(** [add le p] is a player with all the same tiles as [p] 
    in addition to [le]. *)
val add_tile : tile -> t -> t

(** [add_move amount p] is a player with [amount] added to their score*)
val add_score : int -> t -> t

(** [score p] is the score of a player [p]. *)
val score : t -> int

(** [tiles p] is the list of tiles in a players inventory. *)
val tiles : t -> tile list

(** [new_p] is a new player with no moves or tiles. *)
val new_p : t