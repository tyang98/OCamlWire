open ProposedMove
open CompletedMove
open TileInventory

(** [t] is the type representing the gameplay *)
type t

module CM : CompletedMove

val execute : ProposedMove.t -> t -> t 

val query_tile : int -> int -> tile * (CM.t option) 