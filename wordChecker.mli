(** Representation of a word checker in Scrabble.
    This module is responsible for checking whether a word 
    is actually a legitmate Scrabble word from a dictionary. *)

(** [t] is the type of a word checker. *)
type t

(** [check s d] is whether the dictionary [d] contains the word [s]. *)
val check : string -> t -> bool

(** [load_from_file file_path] is a [WordChecker.t] loaded with strings 
    from the file at [file_path] such that each line of the file is treated 
    as a word. *)
val load_from_file : string -> t
