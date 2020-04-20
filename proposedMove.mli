(** [t] is the type representing a proposed move *)
type t 

(** The type of directions representing either across or down *)
type direction 

(** [letters m] is the list of characters representing each letter used 
    for proposed move [m]. *)
val letters : t -> char list

(** [location m] is the tuple representing the coordinate pair location
    on the board of proposed move [m]. *)
val location : t -> (int * int)

(** [direction m] is the direction of the letters placed for the 
    proposed move [m]. *)
val direction  : t -> direction