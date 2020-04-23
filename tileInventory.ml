type tile = Letter of char |  Blank

type t = tile list

let from_file file = failwith "unimplemented"

let next_tile i = 
  match i with
  | h::t -> (Some h, t)
  | [] -> (None, [])