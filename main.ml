open Board
open TrieDictionary
open TileInventory
open Player
open Gameplay
open State
open ProposedMove

(** [player_parse] is the integer representing the number of players
    playing the game. *)
let player_parse number = 
  number |> int_of_string

type parsed_move =  
  | Placement of ProposedMove.t
  | Swap of ProposedSwap.t 

(** [parse move] is the ProposedMove submitted by the user based on a 
    x and y coordinate location, direction, and word. *)
let parse move =
  let chrl_of_str str = List.init (String.length str) (fun i -> str.[i]) in
  let l = String.split_on_char ' ' move in
  match l with 
  | "move"::t -> begin match t with 
      | x::y::d::w::[] -> let dir = d |> function | "a" | "across" -> Across
                                                  | "d" | "down" -> Down 
                                                  | _ -> 
                                                    failwith "Invalid Direction"
        in Placement (ProposedMove.create dir (int_of_string x, int_of_string y) 
                        (chrl_of_str w))
      | _ -> failwith "Invalid mvoe"
    end 
  | "swap"::t -> begin match t with 
      | _ -> Swap (ProposedSwap.create t) end
  | _ -> failwith ""

(** [turn state] is the function that runs each turn of the game. Each
    turn includes placing new letters on the board and returning an associated
    score for each player's respective moves. *)
let rec turn state =
  ANSITerminal.(print_string [Bold] "Current Turn: " );
  print_endline ("Player " ^ (state |> State.whose_turn |> string_of_int));
  ANSITerminal.(print_string [red; Bold] "Your Score: "); 
  (State.whose_turn state |> State.get_player state |> Player.score 
   |> string_of_int |> print_string); print_newline ();
  State.board_printer state;
  print_endline ("Your tiles: " ^ 
                 (state |> State.whose_turn |> State.get_player state 
                  |> Player.tiles |> List.map TileInventory.string_of_tile 
                  |> List.fold_left (fun a b -> a ^ b ^ ";") ""));
  print_string "\n move | swap | pass > ";
  match read_line () with
  | exception End_of_file -> ()
  | move -> match parse move with
    | exception _ -> ANSITerminal.(print_string [red] "Invalid move\n");
      turn state
    | Placement pm -> State.execute pm state |> begin function 
        | None -> ANSITerminal.(print_string [red] "Move not allowed\n\n");
          turn state
        | Some ns -> ns |> State.increment_turn |> turn end
    | Swap sw -> State.swap sw state |> begin function 
        | None -> ANSITerminal.(print_string [red] "Swap not allowed\n\n");
          turn state
        | Some ns -> ns |> State.increment_turn |> turn end

(** [player_count] prompts the user to enter the number of players that
    will play the game. *)
let rec player_count n = 
  match n > 0 with
  | false -> ANSITerminal.(
      print_string [red; Bold]
        "\n Please enter a valid player count: "
    );
    player_count (read_line () |> player_parse)
  | true -> let start = State.init_state n in
    print_endline " ";
    turn start

(** [main ()] prompts for user to play OScrabble, asks the user for the number 
    of players for game, and then starts it. *)
let main () = 
  ANSITerminal.(
    print_string [red; Bold]
      "\n Welcome to OScrabble, a functional implementation of Scrabble. \n\n"
  );
  ANSITerminal.(
    print_string [green; Bold]
      "\n\n How many players would like to play this game? \n"
  );
  ANSITerminal.(
    print_string [green; Bold]
      "\n Enter number of players: "
  );
  let players = read_line () |> player_parse in
  player_count players

let () = main ()
