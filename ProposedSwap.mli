(** [t] is the type representing a proposed move. *)
type t 

(** [tile] is the type representing a [TileInventory] tile. *)
type tile = TileInventory.tile

(** [create tiles] is a proposed swap where we want to swap the tiles 
    represented by the string list [tiles].  Precondition: all strings 
    in the list are of length 1. *)
val create : string list -> t

(** [tiles ps] is the tiles we want to swap. *)
val tiles : t -> tile list

(** [size ps] is the size of the tile list. *)
val size : t -> int