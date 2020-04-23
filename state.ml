open Gameplay
open Board

type t = {
  gameplay : Gameplay.t;
  players : Player.t list;
  current : int;
  tiles : TileInventory.t;
}

(** [bonus_printer tile] is the string representation of a bonus *)
let bonus_printer (bonus : Board.bonus) =
  match bonus with 
  | WordBonus i -> print_string ("WB" ^ (string_of_int i))
  | LetterBonus (i, c) -> print_string ("LB" ^ (string_of_int i))

(** [tile_printer tile] is the string representation of a tile *)
let tile_printer (tile : Board.tile) = 
  match tile with 
  | Filled c -> print_string (Char.escaped c)
  | Bonus b -> bonus_printer b
  | Empty -> print_string " "

(** [list_printer lst] is the string representation of the list [lst] *)
let rec list_printer lst = 
  match lst with 
  | [] -> ()
  | h::t -> tile_printer h; print_string  " "; list_printer t

let board_printer s = 
  let gameplay = s.gameplay in
  let game_board = Gameplay.obtain_board gameplay in
  List.iter (fun lists -> list_printer lists) (game_board |> Board.board)