open OUnit2
open TrieDictionary
open Board
open TileInventory
open Player

(* Test Plan: 
   For our test plan for OScrabble (our implementation of 
   the board game Scrabble), we decided to test the functions from the modules 
   TrieDictionary, Board, TileInventory, State, Player, CompletedMove,
   and ProposedMove with OUnit. On the other hand, we used the terminal by 
   executing (make run) to manually test the functionalities of the Main and 
   Gameplay modules because we could most easily identify the flow of our game 
   and find potential bugs for scoring and the user-interface. In terms of
   our OUnit tests, we developed our test cases 

   Deductions (aka we need to explain)
   -The test plan does not explain which parts of the system were automatically 
   tested by OUnit vs. manually tested. 
   - The test plan does not explain what modules were tested by OUnit and how 
     test cases were developed (black box, glass box, randomized, etc.). 
   - The test plan does not provide an argument for why the testing approach
     demonstrates the correctness of the system.*)

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

let completed_move_scm_test (name : string) (cm : SCM.t ) (e : int) : test = 
  name >:: (fun _ -> assert_equal e (SCM.score cm) ~printer:(string_of_int)) 

let totNoBonus =  SCM.from [("tot", [], [])]
let totDoubleLetter = SCM.from [("tot", [('t', 2)], [])]
let totDoubleWord = SCM.from [("tot", [], [2])]
let tttTripleLetter = SCM.from [("ttt", [('t', 3)], [])]
let tinytomTripleWord = SCM.from [("tinytom", [], [3])]

(* Test CompletedMove functions. *)
let completed_move_tests = [
  completed_move_scm_test "Simple 3 letter test no bonuses" totNoBonus 3;
  completed_move_scm_test "Simple 3 letter double word" totDoubleWord 6;
  completed_move_scm_test 
    "only 1 bonus letter, but 2 letters in word" totDoubleLetter 4;
  completed_move_scm_test 
    "only 1 bonus letter, but 3 letters in word" tttTripleLetter 5;
  completed_move_scm_test "Simple 7 letter triple word" tinytomTripleWord 36;
  completed_move_scm_test 
    "Simple 3 letter test with triple value 1 letter matching" 
    (SCM.from [("got", [('g', 3)], [])]) 8;
  completed_move_scm_test 
    "Simple 4 letter test with double value word bonus" 
    (SCM.from [("heat", [], [2])]) 14;
  completed_move_scm_test "Letter and word bonus"
    (SCM.from [("got", [('g', 3)], [2])]) 16;
  completed_move_scm_test "Two word test"
    (SCM.from [("toe", [], [3]); ("ending", [('i', 2)], [3])]) 36;
]

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
  |> Player.update_tile [Letter 'i'; Letter 'c'; Letter 'e'; Letter 'n']

let the_state = State.init_players [the_player]

let player_4 = State.init_state 4

(* Move 5,6,a,ice *)
let move_simple = ProposedMove.create Across (5,6) ['i';'c';'e'] 

(* move 5,5,a,n to extend move_simple*)
let move_ex = ProposedMove.create Across (4,6) ['n'] 

let legal_move = ProposedMove.create Across (7, 7) ['h';'e';'y']

let illegal_move = ProposedMove.create Across (4, 6) ['a';'f';'d']

(* Test State functions. *)
let state_tests = [
  "Test initial turn" >:: (fun _ -> 
      assert_equal (player_4 |> State.whose_turn) (0));
  "Test turn incrementing works correctly" >:: (fun _ -> 
      assert_equal (player_4 
                    |> State.increment_turn 
                    |> State.increment_turn 
                    |> State.whose_turn) (2));
  "Test turn incrementing wrap around works" >:: (fun _ -> 
      assert_equal (player_4
                    |> State.increment_turn 
                    |> State.increment_turn 
                    |> State.increment_turn
                    |> State.increment_turn 
                    |> State.whose_turn) (0));

  "Test scoring" >:: (fun _ -> 
      assert_equal (Some 8) (the_state
                             |> State.execute move_simple
                             |> Option.map (
                               fun s -> State.get_player s 0 |> Player.score
                             ) ) 
    );

  "Test placing removes tiles from inventory" >:: (fun _ -> 
      assert_equal (Some 1) (the_state
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
      assert_equal (Some 14) (
        (match State.execute move_simple the_state with 
         | Some r -> State.execute move_ex r 
         | None -> None)
        |> Option.map (
          fun s -> State.get_player s 0 |> Player.score)
      )
        ~printer:(fun x -> match x with None -> "none" 
                                      | Some x -> string_of_int x) 
    )
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
  completed_move_tests, true; 
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
