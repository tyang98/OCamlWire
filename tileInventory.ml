type tile = Letter of char |  Blank

type t = tile list

let shuffle ls = 
  List.map (fun x -> (Random.bits (), x)) ls
  |> List.sort (fun (a, _) (b, _) -> compare a b )
  |> List.map snd

let from_file path = 
  let get_line file = try Some (input_line file) with End_of_file -> None in
  let file = open_in path in
  let rec loop acc =
    match get_line file with
    | Some line when not (0 = String.length line) -> line::acc
    | _ -> acc
  in
  let tiles = loop [] in
  close_in file;
  tiles |> shuffle

let next_tile i = 
  match i with
  | h::t -> (Some h, t)
  | [] -> (None, [])