open Gameplay
open Board
open WordChecker
open TileInventory

exception NotBonus

type t = {
  gameplay : Gameplay.t;
  players : Player.t list;
  current : int;
  tiles : TileInventory.t;
}

let default_bonuses = [
  (0,0, WordBonus 3); (0,3, LetterBonus (2)); (0,7, WordBonus 3); 
  (0,11, LetterBonus (2)); (0,14, WordBonus 3); 
  (1,1, WordBonus 2); (1,5, LetterBonus (3)); (1,9, LetterBonus (3));
  (1,13, WordBonus 2); (2,2, WordBonus 2); 
  (2,6, LetterBonus (2)); (2,8, LetterBonus (2)); (2,12, WordBonus 2);
  (3,0, LetterBonus (2)); (3,3, WordBonus 2); (3,7, LetterBonus (2)); 
  (3,13, WordBonus 2); (3,14, LetterBonus (2));
  (4,4, WordBonus 2); (4,10, WordBonus 2);
  (5,1, LetterBonus (3)); (5,5, LetterBonus (3));
  (5,9, LetterBonus (3)); (5,13, LetterBonus (3)); 
  (6,2, LetterBonus (2)); (6,6, LetterBonus (2)); 
  (6,8, LetterBonus (2)); (6,12, LetterBonus (2));
  (7,0, WordBonus 3); (7,3, LetterBonus (2)); (7,7, Start); 
  (7,11, LetterBonus (2)); (7,14, WordBonus 3); 
  (8,2, LetterBonus (2)); (8,6, LetterBonus (2));
  (8,8, LetterBonus (2)); (8,12, LetterBonus (2)); 
  (9,1, LetterBonus (3)); (9,5, LetterBonus (3));
  (9,9, LetterBonus (3)); (9,13, LetterBonus (3));
  (10,4, WordBonus 2); (10,10, WordBonus 2); (11,0, LetterBonus (2));
  (11,3, WordBonus 2); (11,7, LetterBonus (2)); (11,11, WordBonus 2); 
  (11,14, LetterBonus (2)); (12,2, WordBonus 2); 
  (12,6, LetterBonus (2)); (12,8, LetterBonus (2));
  (12,12, WordBonus 2); (13,1, WordBonus 2); (13,5, LetterBonus (3));
  (13,9, LetterBonus (3)); (13,13, WordBonus 2);  
  (14,0, WordBonus 3); (14,3, LetterBonus (2));(14,7, WordBonus 3); 
  (14,11, LetterBonus (2));(14,14, WordBonus 3)]

(** [give_move move players i] gives a completed move [move] to a player in 
    [players] at index [t] and returns the updated list. *)
let give_score (amount : int)
    (players : Player.t list) (i : int) : Player.t list =
  List.mapi (fun  index player -> 
      if (index = i) then Player.add_score amount player else player
    ) players




(** [make_players playerc ps] is a list of [Player.new_p] with a length of 
    playerc. *)
let rec make_players playerc ps = match playerc with
  | 0 -> ps
  | _ -> make_players (playerc - 1) (Player.new_p::ps)


(** [get_n_tiles i n acc] is a list of tiles appended to [acc].  If there 
    are enough tiles left in [i], then [n] tiles will be 
    appended to [acc].  If there are not enough tiles left in [i], then 
    the remaining tiles left in [i] will be appended to [Some acc].  If there 
    are no tiles left at all, then we return [None].  Always pass [[]] to this
    function. *)
let rec get_n_tiles (i : TileInventory.t) (n : int) (acc : tile list) 
  : TileInventory.t * (tile list option)  = 
  match n with 
  | 0 -> if List.length acc = 0 then i, None else  i, Some acc
  | _ -> begin 
      match TileInventory.next_tile i with
      | Some tile, i' -> get_n_tiles i' (n - 1) (tile::acc)
      | None, _ -> get_n_tiles i 0 acc
    end

(** [give_player_tiles pl i tiles] gives the [i]th player in the player list [pl]
    the tiles in [tiles] *)  
let give_player_tiles pl i tiles = 
  List.mapi (fun iter player -> if (i = iter) then List.fold_left 
                  (fun (p : Player.t) (t : tile) -> Player.add_tile t p) 
                  player tiles
              else player) pl

(** [grab_tile s n pn] is Some state [s] where player number [pn] has been given
    [n] new tiles from the tile inventory, if there are tiles left in the 
    inventory.  If there are no more tiles left in the inventory, grab_tile
    is None*)
let grab_tile s n pn = 
  let tile_inventory', tiles = get_n_tiles s.tiles n [] in 
  match tiles with
  | Some tile_list -> {s with 
                       players = 
                         give_player_tiles s.players pn tile_list; 
                       tiles = tile_inventory'
                      }
  | None -> s

(** [get_player s pn] is a player with player number [pn] in state [s]*)
let get_player s pn = match List.nth_opt s.players pn with
  | Some p -> p
  | None -> failwith "Precondition violated: No such player"

let init_state player = 
  List.fold_left (fun  (s : t) (n : int) -> grab_tile s 7 n) {
    gameplay = make_gameplay (Board.init_board default_bonuses 15) 
        (WordChecker.load_from_file "scrabble.txt"); 
    players = make_players player [];
    current = 0;
    tiles =  TileInventory.from_file "tiles.txt"
  } (List.init player (fun x -> x))

let init_players players = {
  gameplay = make_gameplay (Board.init_board default_bonuses 15) 
      (WordChecker.load_from_file "scrabble.txt"); 
  players = players;
  current = 0;
  tiles =  TileInventory.from_file "tiles.txt"
} 


(** [bonus_printer tile] is the string representation of a bonus. *)
let bonus_printer (bonus : Board.bonus) =
  match bonus with 
  | WordBonus 2 -> let wb = ("W" ^ (string_of_int 2)) in
    ANSITerminal.(print_string [red; Bold] wb)
  | WordBonus 3 -> let wb = ("W" ^ (string_of_int 3)) in
    ANSITerminal.(print_string [magenta; Bold] wb)
  | LetterBonus (2) -> let lb =("L" ^ (string_of_int 2)) in
    ANSITerminal.(print_string [cyan; Bold] lb)
  | LetterBonus (3) -> let lb =("L" ^ (string_of_int 3)) in
    ANSITerminal.(print_string [blue; Bold] lb)
  | Start -> ANSITerminal.(print_string [on_red; Bold] "  ")
  | _ -> raise NotBonus

(** [tile_printer tile] is the string representation of a tile. *)
let tile_printer (tile : Board.tile) = 
  match tile with 
  | Filled c -> ANSITerminal.(print_string [green; Bold] 
                                (((Char.escaped c) |> 
                                  String.uppercase_ascii) ^ " "))
  | Bonus b -> bonus_printer b
  | Empty -> print_string "()"

(** [list_printer lst] is the string representation of the list [lst]. *)
let rec list_printer i lst = 
  let num_string = i |> string_of_int  in 
  ANSITerminal.(print_string [Bold] num_string);
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
    let num_string = i |> string_of_int  in 
    ANSITerminal.(print_string [Bold] num_string);
    if i < 10 then print_string "  " else print_string " "
  done;
  print_string "\n";
  List.iteri (fun i lists -> list_printer i lists) (game_board |> Board.board)

(** [whose_turn s] is the player number of whose turn it is*)
let whose_turn s = s.current

(** [increment_turn s] is the state [s] with the current player being 
    incremented by 1, including wrap around.*)
let increment_turn s = 
  {s with current = (s.current + 1) mod (List.length s.players)}

(** [tiles_remaining ls l_tiles blanks] is Some tiles where tiles is the 
    result of removing the letters and blanks required to create the letters
    in [ls] from [l_tiles] and [blanks], and is None if it is not possible
    to remove blanks or letters to match [ls]*)
let rec tiles_remaining (ls : char list) (l_tiles : char list) (blanks : int) 
  : TileInventory.tile list option = 
  match ls with 
  | h::t when List.mem h l_tiles -> 
    tiles_remaining t (List.filter (fun a -> h <> a) l_tiles) blanks
  | h::t when blanks > 0 -> tiles_remaining t l_tiles (blanks - 1)
  | h::t -> None
  | [] -> Some (List.flatten [List.map (fun a -> Letter a) l_tiles; 
                              List.init blanks (fun _ -> Blank)])

(** [verify_tiles move inventory] is Some [move] if all the tiles required to
*)  
let verify_tiles (move : ProposedMove.t) (inventory : TileInventory.tile list) 
  : ProposedMove.t option * (TileInventory.tile list) = 
  let num_blanks, letter_tiles = 
    inventory 
    |> List.partition (fun a -> a == Blank) 
    |> fun (b, l) -> 
    (
      List.length b,
      List.map (fun a -> 
          match a with 
          | Blank -> failwith "Precondition violated" 
          | Letter a -> a) l
    ) in
  let x = tiles_remaining (ProposedMove.letters move) letter_tiles num_blanks in 
  match x with 
  | Some l -> (Some move, l)
  | None -> None, []

let update_tiles (new_tiles : Player.tile list) (pn : int) (s : t) : t option = 
  Some {s with 
        players = List.mapi 
            (fun i p -> 
               if i = pn then Player.update_tile new_tiles p else p) s.players

       }

(* Lets make a quick sort-of-monad operator to make this cleaner *)
let (>>=) (lhs : 'a option) (rhs : 'a -> 'b option) : 'b option = 
  match lhs with
  | Some a -> rhs a
  | None -> None

let execute (move : ProposedMove.t) (e : t) = 
  let (pmove_opt, new_tiles) = 
    verify_tiles move (e |> whose_turn |> get_player e |> Player.tiles) in
  pmove_opt 
  >>= (fun a -> Gameplay.execute a e.gameplay) 
  >>= (fun (gn, score') ->
      Some { 
        e with gameplay = gn; 
               players = give_score score' e.players e.current;
      })
  >>= update_tiles new_tiles e.current

let swap (swap : ProposedSwap.t) (s : t) : t option = 
  let lt = 
    ProposedSwap.tiles swap |> List.map (fun x -> match x with | Letter r -> r 
                                                               | Blank -> '_') 
  in s |> whose_turn |> get_player s |> Player.tiles |> 
     List.partition (fun a -> a == Blank) 
     |> (fun (b, l) ->  tiles_remaining lt
            (List.map (fun x -> 
                 match x with 
                 | Letter r -> r 
                 | Blank -> failwith "precondition violated"
               ) l) 
            (List.length b)
        ) |> function | None -> None
                      | Some l -> Some ( 
                          grab_tile { 
                            s with

                            players =
                              List.mapi 
                                (fun i p -> 
                                   if i = s.current then Player.update_tile l p 
                                   else p) 
                                s.players
                          } (ProposedSwap.size swap) s.current) 
