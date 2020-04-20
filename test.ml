open OUnit2

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

let suite = "search test suite" >::: List.flatten [tests; completed_move_tests]

let _ = run_test_tt_main suite