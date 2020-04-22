open OUnit2
open TrieDictionary
open Board


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
    (SCM.from [("toe", [], [3]); ("ending", [('i', 2)], [3])]) 36
]

let tests = [
]

type bonus = WordBonus of int | LetterBonus of int * char
type tile = {
  x_coord: int;
  y_coord: int;
  filled : char;
  isBonus : bonus option;
}
let tile_1 = {x_coord = 1; y_coord = 2; filled = 'C'; isBonus = None}
let tile_2 = {x_coord = 2;y_coord = 2; 
              filled = 'S'; isBonus = Some (WordBonus 2)}
let tile_3 = {x_coord = 42; y_coord = 36; filled = 'A'; isBonus = None}    

let board = [tile_1; tile_2; tile_3]

let rec query_tile r c b = 
  match b with 
  | [] -> None
  | h::t -> if h.x_coord = r && h.y_coord = c 
    then Some h
    else query_tile r c t

(** [bonus_extract t] is the integer corresponding whether
    a bonus exists on tile [t]. If result is 0 then there is no bonus on 
    tile [t]. If result is 1 then there is a bonus on tile [t]. *)
let rec bonus_extract t =
  match t.isBonus with
  | None -> 0
  | Some (WordBonus a) -> 1
  | Some (LetterBonus (a,b)) -> 1

let rec check_bonus r c b = 
  match b with 
  | [] -> None
  | h::t -> if (h |> bonus_extract) = 1
    then h.isBonus
    else check_bonus r c t

(** [remover r c b] is the list with the tile with 
    located in row [r] and column [c] removed in [b]. *)
let rec remover r c b =
  match b with 
  | [] -> []
  | h::t -> if h.x_coord = r && h.y_coord = c 
    then remover r c t
    else h::(remover r c t)

let set_tile r c l b = 
  let tile = (List.find (fun t -> t.x_coord = r && t.y_coord = c) b) in
  {tile with filled = l}::(remover r c b) |> List.sort compare

let replace_board = [tile_1; {tile_2 with filled = 'H'}; tile_3]

let board_tests = [
  "board query test" >:: (fun _ -> assert_equal (Some tile_2) 
                             (query_tile 2 2 board));
  "board bonus test" >:: (fun _ -> assert_equal (Some (WordBonus 2)) 
                             (check_bonus 2 2 board));
  "board set_tile test" >:: (fun _ -> assert_equal replace_board 
                                (set_tile 2 2 'H' board));
  "board size test" >:: (fun _ -> assert_equal 3 
                            (List.length board));


]

let suite = "scrabble test suite" >::: List.flatten [tests; completed_move_tests; 
                                                     trie_tests; board_tests]

let _ = run_test_tt_main suite
