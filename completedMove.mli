(** [t] is the type representing a completed move*)
type t


(** [score m] is the integer score of the letters
    placed for the completed move [m] *)
val score : t -> int

(** [words m] is the list of words formed for the completed move [m]  *)
val words : t -> string list