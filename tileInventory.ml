type tile = Letter of char | Blank

type t = tile list

(** [shuffle lst] is the list [lst] with its elements placed 
    in a random order. *)
let shuffle lst = 
  List.map (fun x -> (Random.bits (), x)) lst
  |> List.sort (fun (a, _) (b, _) -> compare a b )
  |> List.map snd

let from_file path = 
  let get_line file = try Some (input_line file) with End_of_file -> None in
  let to_tile (c : char) = if c = '_' then Blank else Letter c in
  let file = open_in path in
  let rec loop acc =
    match get_line file with
    | Some line when not (0 = String.length line) -> 
      loop ((line.[0] |> Char.lowercase_ascii |> to_tile)::acc)
    | _ -> acc
  in
  let tiles = loop [] in
  close_in file;
  tiles |> shuffle

let next_tile i = 
  match i with
  | h::t -> (Some h, t)
  | [] -> (None, [])

let draw n t = 
  let rec inner c n t = match n, next_tile t with
    | _, (None, _)
    | 0, _ -> (c, t)
    | _, (Some l, nt) -> inner (l::c) (n - 1) nt
  in inner [] n t

let string_of_tile t = 
  match t with 
  | Letter c -> String.make 1 c 
  | Blank -> "_"

let tile_of_char c =
  match c with
  | '_' -> Blank
  | c -> Letter c

(* Make sure the randomizer is actually initialized to ensure random tiles. *)
let _ = Random.self_init ()