open OUnit2
open TrieDictionary
open Board
open TileInventory
open Player

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

(* let () = print_endline "starting dictionary load"
   let wc = WordChecker.load_from_file "scrabble.txt"
   let () = print_endline "finished dictionary load" *)
(* 
let make_wc_test word =
  word >:: (fun _ -> WordChecker.check word wc
                     |> assert_equal true ~printer:(string_of_bool))

let word_checker_tests = [
  make_wc_test "abacuses";
  make_wc_test "octopus";
  make_wc_test "aa";
  make_wc_test "zzzs"
] *)

module SCM = StandardCompletedMove.StandardCompletedMove

let completed_move_scm_test (name : string) (cm : SCM.t ) (e : int) : test = 
  name >:: (fun _ -> assert_equal e (SCM.score cm) ~printer:(string_of_int)) 

let totNoBonus =  SCM.from [("tot", [], [])]
let totDoubleLetter = SCM.from [("tot", [('t', 2)], [])]
let totDoubleWord = SCM.from [("tot", [], [2])]

let completed_move_tests = [
  completed_move_scm_test "Simple 3 letter test no bonuses" totNoBonus 3;
  completed_move_scm_test "Simple 3 letter double word" totDoubleWord 6;
  completed_move_scm_test 
    "only 1 bonus letter, but 2 letters in word" totDoubleLetter 4;
  completed_move_scm_test 
    "Simple 3 letter test with triple value 1 letter matching" 
    (SCM.from [("got", [('g', 3)], [])]) 8;
  completed_move_scm_test "Letter and word bonus"
    (SCM.from [("got", [('g', 3)], [2])]) 16;
  completed_move_scm_test "Two word test"
    (SCM.from [("toe", [], [3]); ("ending", [('i', 2)], [3])]) 36;
]

let tests = [

]

let bonuses = [(0, 0, WordBonus 0); (1, 2, WordBonus 3)]

let b = init_board [] 15

let b2 = init_board bonuses 15

let b_with_letter = b 
                    |> Board.set_tile 0 0 'a' 
                    |> Board.set_tile 0 14 'b' 
                    |> Board.set_tile 14 14 'c'
                    |> Board.set_tile 14 0 'd'

let board_tests = [
  "Test board of size 15 is actually size 15" >:: (fun _ ->
      assert_equal (Board.size b) (15, 15));
  "Test bonus in 0, 0" >:: (fun _ ->
      assert_equal (Board.check_bonus 0 0 b2) (Some (WordBonus 0)));
  "Test bonus in 1, 2" >:: (fun _ ->
      assert_equal (Board.check_bonus 1 2 b2) (Some (WordBonus 3)));
  "Test letters in 0,0" >:: (fun _ -> 
      assert_equal (Board.query_tile 0 0 b_with_letter) (Some (Filled 'a')));
  "Test letters in 0,15" >:: (fun _ -> 
      assert_equal (Board.query_tile 0 14 b_with_letter) (Some (Filled 'b')));
  "Test letters in 0,0" >:: (fun _ -> 
      assert_equal (Board.query_tile 14 14 b_with_letter) (Some (Filled 'c')));
  "Test letters in 0,0" >:: (fun _ -> 
      assert_equal (Board.query_tile 14 0 b_with_letter) (Some (Filled 'd')));

]

open Player

let player_with_2_moves = 
  Player.new_p |> Player.add_move totNoBonus |> Player.add_move totDoubleLetter
let player_with_1_tile = 
  Player.new_p |> Player.add_tile Blank

let player_tests = [
  "Player score test, multi moves" >:: (fun _ ->
      assert_equal (Player.score player_with_2_moves) 7);
  "New player has score 0" >:: (fun _ ->
      assert_equal (Player.score Player.new_p) 0);
  "New player has no tiles" >:: (fun _ ->
      assert_equal (Player.tiles Player.new_p) []);
  "Test tile adding" >:: (fun _ ->
      assert_equal (Player.tiles player_with_1_tile |> List.length) 1);
]

let state_tests = [
  "Test turn incrementing works correctly" >:: (fun _ -> 
      assert_equal (State.init_state 4 
                    |> State.increment_turn 
                    |> State.increment_turn 
                    |> State.whose_turn) (2));
  "Test turn incrementing wrap around works" >:: (fun _ -> 
      assert_equal (State.init_state 4 
                    |> State.increment_turn 
                    |> State.increment_turn 
                    |> State.increment_turn
                    |> State.increment_turn 
                    |> State.whose_turn) (0));
]

let suite = "scrabble test suite" >::: List.flatten [
    tests;
    completed_move_tests; 
    trie_tests;
    board_tests;
    player_tests;
    (* word_checker_tests; *)
    state_tests;
  ]

let _ = run_test_tt_main suite
