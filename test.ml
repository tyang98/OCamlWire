open OUnit2

let tests = [
  "Test test" >:: (fun _ -> assert_equal 1 0)
]

let suite = "scrabble test suite" >::: tests

let _ = run_test_tt_main suite