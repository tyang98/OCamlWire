(** Representation of a proposed move in Scrabble.
    This module represents a player's intended move with 
    specified locations, directions, and letters. *)

(** [t] is the type representing a proposed move. *)
type t 

(** The type of directions representing either across or down. *)
type direction = Across | Down

(** [create d i lst] is the proposed move given a direction [d] 
    tuple [i] consisting of the row and column space and a list of 
    letters [lst]. *)
val create : direction -> int * int -> char list -> t

(** [letters m] is the list of characters representing each letter used 
    for proposed move [m]. *)
val letters : t -> char list

(** [location m] is the tuple representing the coordinate pair location
    on the board of proposed move [m]. *)
val location : t -> (int * int)

(** [direction m] is the direction of the letters placed for the 
    proposed move [m]. *)
val direction  : t -> direction