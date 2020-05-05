(** [t] is the dictionary used for word verification. *)
type t

(** [check s d] is the boolean that is true if the dictionary [d] contains 
    the word [s], false otherwise. *)
val check : string -> t -> bool

(** TODO: Document *)
val load_from_file : string -> t

(* val load_from_file_definitions : string -> t *)