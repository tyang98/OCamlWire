open ProposedMove
open CompletedMove
open TileInventory

(** [t] is the type representing the gameplay *)
type t
val execute : ProposedMove.t -> t -> t * CompletedMove.t

val query_tile : int -> int -> tile