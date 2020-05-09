(**  Representation of the Scrabble game board.
     This module represents the initial structure of the game board with 
     a specified size and set of bonuses. *)


(** [t] is the type representing a board. *)
type t 

(** [bonus] is the type representing either a letter or word point bonus 
    and starting tile. *)
type bonus = WordBonus of int | LetterBonus of int | Start

(** [tile] is the type of a tile. *)
type tile = Filled of char | Bonus of bonus | Empty 

(** [board t] is the list representation of type [t]. *)
val board : t -> tile list list

(** [init_board l s] is the board with size [s] x [s]. *)
val init_board : (int * int * bonus) list -> int -> t

(** [query_tile r c b] is [Some] tile located in row [r] and column [c] on 
    board [b] if there is a tile in [r] [c], otherwise is [None] if there is 
    no tile in [r] [c]. *)
val query_tile : int -> int ->  t -> tile option

(** [check_bonus r c b] is [Some] bonus if there is a bonus located in [r] [c]
    on board [b], otherwise it is [None] if there is no bonus in [r] [c]. *)  
val check_bonus : int -> int -> t -> bonus option

(** [set_tile r c l b] is a board with all the original letters in [b], but 
    with [l] in spot [r] [c]. *)
val set_tile : int -> int -> char -> t -> t

(** [size b] is the tuple representing (width, height). *)
val size : t -> int * int
