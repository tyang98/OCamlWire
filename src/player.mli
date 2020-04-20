(** [t] is the type representing a player*)
type t 

(** [tile] represents the type of tiles contained in a players inventory *)
type tile = Letter of char |  Blank 

(** [score p] is the score of a player [p]*)
val score : t -> int

(** [tiles p] is the list of tiles in a players inventory*)
val tiles : t -> tile list