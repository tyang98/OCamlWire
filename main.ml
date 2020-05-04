open Board
open TrieDictionary
open TileInventory
open Player
open Gameplay
open State
open ProposedMove

(** TODO: Document *)
let player_parse number = 
  number |> int_of_string

(** TODO: Document *)
let parse move = let l = String.split_on_char ' ' move in
  let x = l |> List.hd |> int_of_string in
  let l = List.tl l in
  let y = l |> List.hd |> int_of_string in
  let l = List.tl l in
  let dir = l |> List.hd |> function
    | "a"
    | "across" -> Across
    | "d"
    | "down" -> Down
    | _ -> failwith "invalid direction"
  in
  let l = List.tl l in
  let word = List.hd l in
  ProposedMove.create dir (x, y)
    (List.init (String.length word) (String.get word))

(** TODO: Document *)
let rec turn state =

  ANSITerminal.(print_string [red;Bold] "Current Turn: " );
  print_endline ("Player " ^ (state |> State.whose_turn |> string_of_int));
  ANSITerminal.(print_string [red; Bold] "Your Score: "); 
  (State.whose_turn state |> State.get_player state |> Player.score 
   |> string_of_int |> print_string); print_newline ();
  State.board_printer state;
  print_endline ("Your tiles: " ^ 
                 (
                   state |> State.whose_turn |> State.get_player state 
                   |> Player.tiles |> List.map TileInventory.string_of_tile 
                   |> List.fold_left (fun a b -> a ^ b ^ ";") ""));
  print_string "\n move > ";
  match read_line () with
  | exception End_of_file -> ()
  | move -> match parse move with
    | exception _ -> ANSITerminal.(print_string [red] "Invalid move\n");
      turn state
    | pm -> State.execute pm state |> function
      | None -> ANSITerminal.(print_string [red] "Move not allowed\n");
        turn state
      | Some ns -> ns |> State.increment_turn |> turn

(** TODO: Document *)
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

(** TODO: Document *)
let main () = 
  ANSITerminal.(
    print_string [red; Bold]
      "\n\n Welcome to OScrabble, a functional implementation of Scrabble. \n\n"
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
