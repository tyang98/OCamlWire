open OUnit2
open TrieDictionary
open Board
open TileInventory
open Player

(* For our test plan for OScrabble (our implementation of 
   the board game Scrabble), we decided to test the functions from the modules 
   TrieDictionary, Board, TileInventory, State, Player, CompletedMove,
   and WordChecker with OUnit. On the other hand, we used the terminal by 
   executing (make run) to manually test the functionalities of the Main and 
   Gameplay modules because we could most easily identify the flow of our game 
   and find potential bugs for scoring and the user-interface.

   In terms of our OUnit tests, we developed our test cases primarily under 
   the principle of black box testing. Our test cases involved both typical 
   inputs and  boundary cases to check to see whether our functions worked 
   properly. 
   For the TrieDictionary module, we tested the insertion of words and point
   values into the data structure and checked after a series of operations
   whether we could retrive the point value of the associated word. 
   For the Board module, we tested the initialization of our game board. 
   Moreover, we verified searching for tiles on the edge of the board 
   and for tiles in the middle of the board in order to see whether we could 
   retrieve the correct information from all locations of the board. 
   For the TileInventory module, we tested whether we could parse a file of
   tiles and verified the type of tile we had after traversing through 
   the list of tiles. We did this with a file of only blank tiles because
   our system involves the random shuffling of tiles after parsing so it 
   would have been difficult to find out which tile was next. 
   For the State module, we tested for the scoring of the game, the changing
   of player turns, and the placing of tiles on the board. We made sure that
   players could receive points for placing down a valid word during their
   turn. 
   For the Player module, we tested for the score of an individual player
   and the placement of tiles. We made sure that a player could run out of 
   tiles and could place multiple tiles. 
   For the CompletedMove module, we tested the scoring functionality of
   placing words on bonuses, both Letter and Word bonuses. We made sure
   that the bonus multiplier would work properly for the scoring depending on 
   the type of bonus. 
   For the WordChecker module, we loaded the file containing all the words
   from a scrabble dictionary found online and tested to see whether our
   functions could properly identify valid words. 

   We believe that the testing approach displayed by this testing suite 
   demonstrates the correctness of the underlying logic and structure
   of our game.  We rigorously tested the functionality for each module
   and whether its functions could perform our specification, such as board 
   initialization and word verification. *)

(** [string_to_list s] is a list of characters from the string [s]. *)
let string_to_list s = List.init (String.length s) (String.get s)

module CTD = TrieDictionary.Make(struct
    module Target = Char

    type t = string

    let to_list = string_to_list
  end)

let complex_trie = CTD.empty
                   |> CTD.insert "hello" 1
                   |> CTD.insert "hi" 2
                   |> CTD.insert "test" 3
                   |> CTD.insert "testing" 4
                   |> CTD.insert "beep" 5
                   |> CTD.insert "hello" 10
                   |> CTD.insert "hi" 20
                   |> CTD.insert "jack" 30
                   |> CTD.insert "tony" 40
                   |> CTD.insert "chris" 50

(* Test trieDictionary functions. *)
let trie_tests = [
  "insert 1" >:: (fun _ -> CTD.empty |> CTD.insert "test" 5
                           |> CTD.get "test"
                           |> assert_equal (Some 5));
  "insert twice " >:: (fun _ -> CTD.empty
                                |> CTD.insert "test" 5
                                |> CTD.insert "test" 5
                                |> CTD.get "test"
                                |> assert_equal (Some 5));
  "insert replace " >:: (fun _ -> CTD.empty
                                  |> CTD.insert "test" 5
                                  |> CTD.insert "test" 20
                                  |> CTD.get "test"
                                  |> assert_equal (Some 20));
  "insert sub get sub" >:: (fun _ -> CTD.empty
                                     |> CTD.insert "test" 5
                                     |> CTD.insert "tes" 20
                                     |> CTD.get "tes"
                                     |> assert_equal (Some 20));
  "insert sub get sub other order" >:: (fun _-> CTD.empty
                                                |> CTD.insert "tes" 5
                                                |> CTD.insert "test" 20
                                                |> CTD.get "tes"
                                                |> assert_equal (Some 5));
  "insert sub get super" >:: (fun _ -> CTD.empty
                                       |> CTD.insert "test" 5
                                       |> CTD.insert "tes" 20
                                       |> CTD.get "test"
                                       |> assert_equal (Some 5));
  "complex get hello" >:: (fun _ -> complex_trie
                                    |> CTD.get "hello"
                                    |> assert_equal (Some 10));
  "complex get hi" >:: (fun _ -> complex_trie
                                 |> CTD.get "hi"
                                 |> assert_equal (Some 20));
  "complex get test" >:: (fun _ -> complex_trie
                                   |> CTD.get "test"
                                   |> assert_equal (Some 3));
  "complex get testing" >:: (fun _ -> complex_trie
                                      |> CTD.get "testing"
                                      |> assert_equal (Some 4));
  "complex get beep" >:: (fun _ -> complex_trie
                                   |> CTD.get "beep"
                                   |> assert_equal (Some 5));
  "complex get jack" >:: (fun _ -> complex_trie
                                   |> CTD.get "jack"
                                   |> assert_equal (Some 30));
  "complex get tony" >:: (fun _ -> complex_trie
                                   |> CTD.get "tony"
                                   |> assert_equal (Some 40));
  "complex get chris" >:: (fun _ -> complex_trie
                                    |> CTD.get "chris"
                                    |> assert_equal (Some 50));
]

let () = print_endline "starting dictionary load"
let wc = WordChecker.load_from_file "scrabble.txt"
let () = print_endline "finished dictionary load" 
let make_wc_test word =
  word >:: (fun _ -> WordChecker.check word wc
                     |> assert_equal true ~printer:(string_of_bool))

(* Test WordChecker functions. *)
let word_checker_tests = [
  make_wc_test "abacuses";
  make_wc_test "octopus";
  make_wc_test "aa";
  make_wc_test "zzzs"
] 
module SCM = StandardCompletedMove.StandardCompletedMove

let bonuses = [(0, 0, WordBonus 0); (1, 2, WordBonus 3)]

let b = init_board [] 15

let b2 = init_board bonuses 15

let b_with_letter = b 
                    |> Board.set_tile 0 0 'a' 
                    |> Board.set_tile 0 14 'b' 
                    |> Board.set_tile 14 14 'c'
                    |> Board.set_tile 14 0 'd'
                    |> Board.set_tile 3 4 'F'

(* Test Board functions. *)
let board_tests = [
  "Test board of size 15 is actually size 15" >:: (fun _ ->
      assert_equal (Board.size b) (15, 15));
  "Test bonus in 0, 0" >:: (fun _ ->
      assert_equal (Board.check_bonus 0 0 b2) (Some (WordBonus 0)));
  "Test bonus in 1, 2" >:: (fun _ ->
      assert_equal (Board.check_bonus 1 2 b2) (Some (WordBonus 3)));
  "Test letters in 0,0" >:: (fun _ -> 
      assert_equal (Board.query_tile 0 0 b_with_letter) (Some (Filled 'a')));
  "Test letters in 0,14" >:: (fun _ -> 
      assert_equal (Board.query_tile 0 14 b_with_letter) (Some (Filled 'b')));
  "Test letters in 14,14" >:: (fun _ -> 
      assert_equal (Board.query_tile 14 14 b_with_letter) (Some (Filled 'c')));
  "Test letters in 14,0" >:: (fun _ -> 
      assert_equal (Board.query_tile 14 0 b_with_letter) (Some (Filled 'd')));
  "Test letters in non-edge position" >:: (fun _ -> 
      assert_equal (Board.query_tile 3 4 b_with_letter) (Some (Filled 'F')));
]

let player_with_2_moves = 
  Player.new_p |> Player.add_score 3 |> Player.add_score 4
let player_with_1_tile = 
  Player.new_p |> Player.add_tile Blank
let player_with_multiple_tiles = 
  Player.new_p |> Player.add_tile Blank |> Player.add_tile (Letter 'A') 
  |> Player.add_tile Blank

(* Test Player functions. *)
let player_tests = [
  "Player score test, multi moves" >:: (fun _ ->
      assert_equal (Player.score player_with_2_moves) 7);
  "New player has score 0" >:: (fun _ ->
      assert_equal (Player.score Player.new_p) 0);
  "New player has no tiles" >:: (fun _ ->
      assert_equal (Player.tiles Player.new_p) []);
  "Test single tile adding" >:: (fun _ ->
      assert_equal (Player.tiles player_with_1_tile |> List.length) 1);
  "Test multiple tile adding" >:: (fun _ ->
      assert_equal (Player.tiles player_with_multiple_tiles |> List.length) 3);
]

let the_player = 
  Player.new_p 
  |> Player.update_tile [Letter 'i'; Letter 'c'; Letter 'e'; Letter 'n';
                         Letter 'i'; Letter 'y'; Letter 'a'; Letter 'w'; 
                         Letter 'a'; Letter 'e'; Letter 'r'; Letter 'a';
                         Letter 'n'; Letter 't';]

let the_state = State.init_players [the_player]

let player_4 = State.init_state 4

(* Move 7,7,a,ice *)
let move_simple = ProposedMove.create Across (7,7) ['i';'c';'e'] 

(* move 6,7,a,n to extend move_simple*)
let move_ex = ProposedMove.create Across (6,7) ['n'] 
let move_ex2 = ProposedMove.create Down (6, 6) ['i']
let move_ex3 = ProposedMove.create Across (4, 5) ['y';'a';'w']
let move_ex4 = ProposedMove.create Down (4,4) ['a';'y';'e']
let move_ex5 = ProposedMove.create (Down) (10,7) ['r';'a';'n';'t']

let legal_move = ProposedMove.create Across (7, 7) ['x';'i']

let illegal_move = ProposedMove.create Across (4, 6) ['a';'f';'d']

(* State.execute operator to work with state options *)
let (>>) (lhs : State.t option) (rhs : ProposedMove.t) : State.t option = 
  match lhs with 
  | Some s -> State.execute rhs s
  | None -> lhs
(** [get_score_p0 state] is the score of player 0 in state [state]*)
let get_score_p0 state = match state with
  | Some state -> State.get_player state 0 |> Player.score
  | None -> -1

(* Test State functions. *)
let state_tests = [
  "Test initial turn" >:: (fun _ -> 
      assert_equal (player_4 |> State.whose_turn) (0));
  "Test turn incrementing works correctly" >:: (fun _ -> 
      assert_equal (player_4 
                    |> State.increment_turn |> State.increment_turn 
                    |> State.whose_turn) (2));
  "Test turn incrementing wrap around works" >:: (fun _ -> 
      assert_equal (player_4
                    |> State.increment_turn |> State.increment_turn 
                    |> State.increment_turn |> State.increment_turn 
                    |> State.whose_turn) (0));
  "Test scoring" >:: (fun _ -> 
      assert_equal (Some 5) (the_state
                             |> State.execute move_simple
                             |> Option.map (
                               fun s -> State.get_player s 0 |> Player.score
                             ))
    );

  "Test placing removes tiles from inventory" >:: (fun _ -> 
      assert_equal (Some (11)) (the_state
                                |> State.execute move_simple
                                |> Option.map (
                                  fun s -> State.get_player s 0 |> Player.tiles 
                                           |> List.length
                                ) ) 
        ~printer:(fun x -> match x with None -> "none" 
                                      | Some x -> string_of_int x)
    );

  "Test invalid word" >:: (fun _ -> 
      assert_equal (None) (State.execute illegal_move the_state) 
    );

  "Test valid word, but dont have the letters" >:: (fun _ -> 
      assert_equal (None) (State.execute legal_move the_state)
    );

  "Test scoring addition" >:: (fun _ -> 
      assert_equal (11) (
        Some the_state >> move_simple >> move_ex |> get_score_p0
      ) ~printer:string_of_int
    );

  "Test scoring addition double letter" >:: (fun _ -> 
      assert_equal (14) (
        Some the_state
        >> move_simple >> move_ex >> move_ex2 |> get_score_p0
      ) ~printer:string_of_int
    );
  "Test scoring addition triple letter" >:: (fun _ -> 
      assert_equal (31) (
        Some the_state
        >> move_simple >> move_ex >> move_ex2 >> move_ex3 |> get_score_p0
      ) ~printer:string_of_int
    );

  "Test scoring addition double word" >:: (fun _ -> 
      assert_equal (46) (
        Some the_state
        >> move_simple >> move_ex >> move_ex2 >> move_ex3 >> move_ex5
        |> get_score_p0
      ) ~printer:string_of_int
    );
]

(* Test TileInventory functions. *)
let tile_inventory_tests = [
  "Test loading from file" >:: (fun _ -> 
      assert_equal (TileInventory.from_file "all_blanks.txt" 
                    |> TileInventory.next_tile |> fst) (Some (Blank)));
  "Test loading from file and taking second" >:: (fun _ -> 
      assert_equal (TileInventory.from_file "all_blanks.txt" 
                    |> TileInventory.next_tile |> snd
                    |> TileInventory.next_tile |> fst) (Some (Blank)));

  "Test loading from file and almost depleating" >:: (fun _ -> 
      assert_equal (TileInventory.from_file "all_blanks.txt" 
                    |> TileInventory.next_tile |> snd
                    |> TileInventory.next_tile |> snd
                    |> TileInventory.next_tile |> snd
                    |> TileInventory.next_tile |> fst) (Some (Blank)));
  "Test loading from file and depleating" >:: (fun _ -> 
      assert_equal (TileInventory.from_file "all_blanks.txt" 
                    |> TileInventory.next_tile |> snd
                    |> TileInventory.next_tile |> snd
                    |> TileInventory.next_tile |> snd
                    |> TileInventory.next_tile |> snd
                    |> TileInventory.next_tile |> fst) (None));
]

(* Set to true to enable a test set. *)
let test_sets = [
  trie_tests, true;
  board_tests, true;
  player_tests, true;
  word_checker_tests, true;
  state_tests, true;
  tile_inventory_tests, true;
]

let suite = "scrabble test suite" >::: 
            (List.filter_map 
               (fun (f, e) -> if e then Some f else None) test_sets 
             |> List.concat)

let _ = run_test_tt_main suite
