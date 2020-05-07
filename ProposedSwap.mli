(** [t] is the type representing a proposed move. *)
type t 

type tile = TileInventory.tile

(** [create tiles] is a proposed swap where we want to swap [tiles]*)
val create : TileInventory.tile list -> t

(** [tiles ps] is the tiles we want to swap*)
val tiles : t -> tile list

(** [size ps] is the size of the tile list*)
val size : t -> int