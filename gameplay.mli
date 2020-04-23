open ProposedMove
open CompletedMove

(** [t] is the type representing the gameplay *)
type t

module CM : CompletedMove

val execute : ProposedMove.t -> t -> t * (CM.t option) 

val query_tile : int -> int -> t -> Board.tile option