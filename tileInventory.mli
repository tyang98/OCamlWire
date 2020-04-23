(** [t] is the type of a tile inventory*)
type t

(** [tile] is the type of a tile*)
type tile

(** [next_tile t] is the next tile to be drawn from the tile inventory, 
    followed by a tile inventory with the tile [tile] removed *)
val next_tile : t -> tile option * t