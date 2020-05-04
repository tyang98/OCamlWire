(** [t] is the type representing a player*)
type t 

(** [tile] represents the type of tiles contained in a players inventory *)
type tile = Letter of char | Blank

module CM = StandardCompletedMove.StandardCompletedMove

(** [add le p] is a player with all the same tiles as [p] in addition to [le] *)
val add_tile : tile -> t -> t

(** [add_move move p] is a player with all the same moves as p with
    the addition of [p] *)
val add_move : CM.t -> t -> t

(** [score p] is the score of a player [p]*)
val score : t -> int

(** [tiles p] is the list of tiles in a players inventory*)
val tiles : t -> tile list

(** [new_p] is a new player with no moves or tiles*)
val new_p : t