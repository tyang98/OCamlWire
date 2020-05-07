(** A dictionary for efficiently checking if keys exist, and optionally
    retrieving associated values. *)
module type Dictionary = sig

  (** The type of an entry in the dictionary. *)
  type entry

  (** The type of a Dictionary that maps [entry] to ['a]. *)
  type 'a t

  (** [empty] is an empty dictionary. *)
  val empty : 'a t

  (** [insert entry value dict] is [dict] updated to contain [entry] and
      [value]. If [entry] already exists, [value] replaces its 
      associated value. *)
  val insert : entry -> 'a -> 'a t -> 'a t

  (** [get entry dict] is an [option] containing the value associated with
      [entry], or [None] if [entry] is not in [dict]. *)
  val get : entry -> 'a t -> 'a option

  (** [check entry dict] is whether or not [dict] contains [entry]. *)
  val check : entry -> 'a t -> bool
end