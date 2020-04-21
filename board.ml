
type bonus = WordBonus of int | LetterBonus of int * char

type tile = {
  x_coord: int;
  y_coord: int;
  filled : char;
  isBonus : bonus option;
}

type t = tile list

let rec query_tile r c b = 
  match b with 
  | [] -> None
  | h::t -> if h.x_coord = r && h.y_coord = c 
    then Some h
    else query_tile r c t

(** [bonus_extract t] is the integer corresponding whether
    a bonus exists on tile [t]. If result is 0 then there is no bonus on 
    tile [t]. If result is 1 then there is a bonus on tile [t]. *)
let rec bonus_extract t =
  match t.isBonus with
  | None -> 0
  | Some (WordBonus a) -> 1
  | Some (LetterBonus (a,b)) -> 1

let rec check_bonus r c b = 
  match b with 
  | [] -> None
  | h::t -> if (h |> bonus_extract) = 1
    then h.isBonus
    else check_bonus r c t

(** [remover r c b] is the list with the tile with 
    located in row [r] and column [c] removed in [b]. *)
let rec remover r c b =
  match b with 
  | [] -> []
  | h::t -> if h.x_coord = r && h.y_coord = c 
    then remover r c t
    else h::(remover r c t)

let set_tile r c l b = 
  let tile = (List.find (fun t -> t.x_coord = r && t.y_coord = c) b) in
  {tile with filled = l}::(remover r c b) |> List.sort compare

let size b = 
  List.length b
