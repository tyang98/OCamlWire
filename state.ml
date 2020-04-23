open Gameplay
open Board
open WordChecker
open TileInventory

type t = {
  gameplay : Gameplay.t;
  players : Player.t list;
  current : int;
  tiles : TileInventory.t;
}

(** [make_players playerc ps] is a list of [Player.new_p] with a length of 
    playerc *)
let rec make_players playerc ps = match playerc with
  | 0 -> ps
  | _ -> make_players (playerc - 1) (Player.new_p::ps)

let init_state player = 
  let start = [] in
  {
    gameplay = make_gameplay (Board.init_board start 15) 
        (WordChecker.load_from_file "scrabble.txt"); 
    players = make_players player [];
    current = 0;
    tiles =  TileInventory.from_file "tiles.txt"
  }

(** [bonus_printer tile] is the string representation of a bonus *)
let bonus_printer (bonus : Board.bonus) =
  match bonus with 
  | WordBonus i -> print_string ("W" ^ (string_of_int i))
  | LetterBonus (i, c) -> print_string ("L" ^ (string_of_int i))

(** [tile_printer tile] is the string representation of a tile *)
let tile_printer (tile : Board.tile) = 
  match tile with 
  | Filled c -> print_string ((Char.escaped c) ^ " ")
  | Bonus b -> bonus_printer b
  | Empty -> print_string "##"

(** [list_printer lst] is the string representation of the list [lst] *)
let rec list_printer lst = 
  match lst with 
  | [] -> print_string "\n"
  | h::t -> tile_printer h; print_string  " "; list_printer t

let board_printer s = 
  let gameplay = s.gameplay in
  let game_board = Gameplay.obtain_board gameplay in
  List.iter (fun lists -> list_printer lists) (game_board |> Board.board)