open Board
open TrieDictionary
open TileInventory
open Player
open Gameplay
open State
open ProposedMove

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

let rec turn state =
  State.board_printer state;
  print_string "\n move > ";
  match read_line () with
  | exception End_of_file -> ()
  | move -> match parse move with
    | exception _ -> ANSITerminal.(print_string [red] "Invalid move\n");
      turn state
    | pm -> State.execute pm state |> function
      | None -> ANSITerminal.(print_string [red] "Move not allowed\n");
        turn state
      | Some ns -> turn ns

let main () = 
  ANSITerminal.(
    print_string [red; Bold]
      "\n\n Welcome to OScrabble, a functional implementation of Scrabble. \n\n"
  );
  let start = State.init_state 1 in
  turn start

let () = main ()
