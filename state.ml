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

let execute move t = Gameplay.execute move t.gameplay
                     |> Option.map (fun g -> print_int (snd g); print_newline; { t with gameplay = fst g; })

(** [make_players playerc ps] is a list of [Player.new_p] with a length of 
    playerc *)
let rec make_players playerc ps = match playerc with
  | 0 -> ps
  | _ -> make_players (playerc - 1) (Player.new_p::ps)


(* Board Config:
   Double Word Score:
   (1,1)  (2,2)  (3,3)  (4,4)  (10,4)  (11,3)  (12,2)  (13,1)
   (1,13) (2,12) (3,11) (4,10) (10,10) (11,11) (12,12) (13,13) 
   Triple Word Score:
   (0,0)  (7,0)  (14,0)
   (0,7)  (7,14) (14,7)
   (0,14)        (14,14)

    Double Letter Score:
   (0,3)  (2,6) (3,0)  (6,2) (7,3)  (8,2) (11,0) (12,6) (14,3) 
   (0,11) (2,8) (3,7)  (6,6) (7,11) (8,6) (11,7) (12,8) (14,11)
                (3,14) (6,8)        (8,8) (11,14)
                       (6,12)       (8,12)

   Triple Letter Score:
   (1,5)   (5,1)  (9,1)  (13,5)
   (1,9)  (5,5)  (9,5)  (13,9)
          (5,9) (9,9)
          (5,13) (9,13) 
*)

let init_state player = 
  let start = [
    (0,0, WordBonus 3); (0,3, LetterBonus (2,' ')); (0,7, WordBonus 3); 
    (0,11, LetterBonus (2,' ')); (0,14, WordBonus 3); 
    (1,1, WordBonus 2); (1,5, LetterBonus (3,' ')); (1,9, LetterBonus (3,' '));
    (1,13, WordBonus 2); (2,2, WordBonus 2); 
    (2,6, LetterBonus (2,' ')); (2,8, LetterBonus (2,' ')); (2,12, WordBonus 2);
    (3,0, LetterBonus (2,' ')); (3,3, WordBonus 2); (3,7, LetterBonus (2,' ')); 
    (3,13, WordBonus 2); (3,14, LetterBonus (2,' '));
    (4,4, WordBonus 2); (4,10, WordBonus 2);
    (5,1, LetterBonus (3,' ')); (5,5, LetterBonus (3,' '));
    (5,9, LetterBonus (3,' ')); (5,13, LetterBonus (3,' ')); 
    (6,2, LetterBonus (2,' ')); (6,6, LetterBonus (2,' ')); 
    (6,8, LetterBonus (2,' ')); (6,12, LetterBonus (2,' '));
    (7,0, WordBonus 3); (7,3, LetterBonus (2,' ')); (7,11, LetterBonus (2,' ')); 
    (7,14, WordBonus 3); (8,2, LetterBonus (2,' ')); (8,6, LetterBonus (2,' '));
    (8,8, LetterBonus (2,' ')); (8,12, LetterBonus (2,' ')); 
    (9,1, LetterBonus (3,' ')); (9,5, LetterBonus (3,' '));
    (9,9, LetterBonus (3,' ')); (9,13, LetterBonus (3,' '));
    (10,4, WordBonus 2); (10,10, WordBonus 2); (11,0, LetterBonus (2,' '));
    (11,3, WordBonus 2); (11,7, LetterBonus (2,' ')); (11,11, WordBonus 2); 
    (11,14, LetterBonus (2,' ')); (12,2, WordBonus 2); 
    (12,6, LetterBonus (2,' ')); (12,8, LetterBonus (2,' '));
    (12,12, WordBonus 2); (13,1, WordBonus 2); (13,5, LetterBonus (3,' '));
    (13,9, LetterBonus (3,' ')); (13,13, WordBonus 2);  
    (14,0, WordBonus 3); (14,3, LetterBonus (2,' '));(14,7, WordBonus 3); 
    (14,11, LetterBonus (2,' '));(14,14, WordBonus 3)] in
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
  | Empty -> print_string "()"

(** [list_printer lst] is the string representation of the list [lst] *)
let rec list_printer i lst = 
  i |> string_of_int |> print_string;
  if i < 10 then print_string "  " else print_string " ";
  List.iter (fun a -> tile_printer a; print_string " ") lst;
  print_string "\n"
(* match lst with  *)
(* | [] -> print_string "\n" *)
(* | h::t -> tile_printer h; print_string  " "; list_printer (i - 1) t *)

let board_printer s = 
  let gameplay = s.gameplay in
  let game_board = Gameplay.obtain_board gameplay in
  print_string "   ";
  for i = 0 to (Board.size game_board |> snd) - 1 do 
    i |> string_of_int |> print_string;
    if i < 10 then print_string "  " else print_string " "
  done;
  print_string "\n";
  List.iteri (fun i lists -> list_printer i lists) (game_board |> Board.board)