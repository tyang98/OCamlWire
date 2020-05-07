open ProposedMove

(** [t] is the type representing the gameplay. *)
type t

(** [execute pm t] is [Some (new_state * move_score)] if the proposed move is 
    valid, otherwise is [None]. *)
val execute : ProposedMove.t -> t -> (t * int) option

(** [query_tile r c g] is Some tile if there is a tile in [r] [c] on [b], 
    otherwise is None. *)
val query_tile : int -> int -> t -> Board.tile option

(** [obtain_board g] is the board corresponding to this gameplay [g]. *)
val obtain_board : t -> Board.t

(** [make_gameplay b w] is a gameplay with a board [b] and 
    a WordChecker [w]. *)
val make_gameplay : Board.t -> WordChecker.t -> t
