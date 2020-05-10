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

(** [parsed_move] is the type representing a Scrabble move. *)
type parsed_move =  
  | Placement of ProposedMove.t list
  | Swap of ProposedSwap.t 
  | Pass
  | Surrender 

(** [player_index player players] is the index of [player] in the list of 
    players [players]. *)
let rec player_index player players =
  match players with
  | [] -> failwith "no player index"
  | h::t -> if h = player then 0
    else 1 + player_index player t

(** [winner_determiner state] is the message to whoever wins the game. *)
let winner_determiner state = 
  try
    let players = State.get_player_list state in
    let best =
      List.find (fun p -> 0 = (p |> Player.tiles |> List.length)) players in
    let index = player_index best players in
    let string_index = (index |> string_of_int) in
    ANSITerminal.(print_string [Bold] 
                    ("\nWinner: " ^ "Player " ^ string_index))
  with _ -> 
    ANSITerminal.(print_string [Bold] ("\nNo winners"))


(** [next_state state] is the new state after a player's turn ends. *)
let next_state state =
  State.increment_turn state

(** [display_final_score num players state] is the representation of the 
    each player's final score in the game's final state [state]. *)
let rec display_final_score num players state = 
  match players with
  | [] ->
    ANSITerminal.(print_string [Bold;green] "\nThanks for playing!!! \n");
    Stdlib.exit 0
  | h::t when num = 1 ->  
    winner_determiner state;
    ANSITerminal.(print_string [Bold; red] "\nFinal Scores: \n");
    print_endline ("Player " ^ (state |> State.whose_turn |> string_of_int)
                   ^ ": " ^ (State.whose_turn state |> State.get_player state 
                             |> Player.score |> string_of_int ));
    display_final_score (num - 1) t (next_state state)
  | h::t ->  
    print_endline ("Player " ^ (state |> State.whose_turn |> string_of_int)
                   ^ ": " ^ (State.whose_turn state |> State.get_player state 
                             |> Player.score |> string_of_int ));
    display_final_score 0 t (next_state state)

(** [parse move] is the ProposedMove submitted by the user based on a 
    x and y coordinate location, direction, and word. *)
let parse move =
  let chrl_of_str str = List.init (String.length str) (fun i -> str.[i]) in
  let rec parse_move dir l = function
    | [] -> l
    | x::y::w::t -> 
      parse_move dir 
        ((ProposedMove.create dir (int_of_string x, int_of_string y) 
            (chrl_of_str w))::l) t
    | _ -> failwith "Invalid move"
  in
  let in_line =
    List.fold_left
      (fun acc move -> let (x, y) = ProposedMove.location move in
        match acc with
        | Some v -> begin match ProposedMove.direction move with
            | Down when x = v -> Some v
            | Across when y = v -> Some v
            | _ -> failwith "Invalid move"
          end
        | None -> match ProposedMove.direction move with
            Across -> Some y | Down -> Some x
      ) None in
  let l = String.split_on_char ' ' move in
  match l with 
  | "move"::t -> begin match t with 
      | d::t when List.length t > 0 -> 
        let dir = d |> function | "a" | "across" -> Across
                                | "d" | "down" -> Down 
                                | _ -> 
                                  failwith "Invalid Direction" in 
        let move = parse_move dir [] t in
        ignore (in_line move);
        Placement move
      | _ -> failwith "Invalid move"
    end 
  | "swap"::t -> begin match t with 
      | _ -> Swap (ProposedSwap.create t) end
  | "pass"::t ->  begin match t with
      | _ -> Pass end
  | "surrender"::t -> begin match t with 
      | _ -> Surrender end
  | _ -> failwith "Not a parsable move"

(** [turn state] is the function that runs each turn of the game. Each
    turn includes placing new letters on the board and returning an 
    associated score for each player's respective moves. *)
let rec turn state =
  if State.game_over state 
  then 
    display_final_score 1 (get_player_list state) state
  else
    ANSITerminal.(print_string [Bold] "\nCurrent Turn: " );
  print_endline ("Player " ^ (state |> State.whose_turn |> string_of_int));
  ANSITerminal.(print_string [red; Bold] "Your Score: "); 
  (State.whose_turn state |> State.get_player state |> Player.score 
   |> string_of_int |> print_string); print_newline ();
  State.board_printer state;
  ANSITerminal.(print_string [Bold] "\n Your tiles: ");
  ANSITerminal.(print_string []
                  (state |> State.whose_turn |> State.get_player state 
                   |> Player.tiles |> List.map TileInventory.string_of_tile 
                   |> List.fold_left (fun a b -> a ^ b ^ ";") ""));
  ANSITerminal.(print_string [Bold] "\n\n move | swap | pass | surrender > ");
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
    | Pass -> State.pass state |> begin function
        | Some ns -> ns |> State.increment_turn |> turn 
        | _ -> ANSITerminal.(print_string [red] "Pass not valid\n\n");
          turn state end
    | Surrender -> State.surrender state |> begin function
        | None -> 
          ANSITerminal.(print_string [red] "Surrender not allowed\n\n");
          turn state 
        | Some ns -> ns |> State.increment_turn |> turn end


(** [player_count n] prompts the user to enter the number of players that
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
      "\n Welcome to OScrabble, a functional implementation of Scrabble. \n"
  );
  ANSITerminal.(print_string [green; Bold]
                  "\n\n How many players would like to play this game? \n");
  ANSITerminal.(print_string [green; Bold]
                  "\n Enter number of players: ");
  let players = read_line () |> player_parse in
  player_count players

let () = main ()
