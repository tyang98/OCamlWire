type tile = Letter of char |  Blank

type t = tile list

let next_tile i = 
  match i with
  | h::t -> (Some h, t)
  | [] -> (None, [])