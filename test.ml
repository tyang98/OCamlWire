open OUnit2
open TrieDictionary

let string_to_list s = List.init (String.length s) (String.get s)

module CTD = TrieDictionary.Make(Char)

let complex_trie = CTD.empty
                   |> CTD.insert (string_to_list "hello") 1
                   |> CTD.insert (string_to_list "hi") 2
                   |> CTD.insert (string_to_list "test") 3
                   |> CTD.insert (string_to_list "testing") 4
                   |> CTD.insert (string_to_list "beep") 5
                   |> CTD.insert (string_to_list "hello") 10
                   |> CTD.insert (string_to_list "hi") 20
                   |> CTD.insert (string_to_list "jack") 30
                   |> CTD.insert (string_to_list "tony") 40
                   |> CTD.insert (string_to_list "chris") 50

let trie_tests = [
  "insert 1" >:: (fun _ -> CTD.empty |> CTD.insert (string_to_list "test") 5
                           |> CTD.get (string_to_list "test")
                           |> assert_equal (Some 5));
  "insert twice " >:: (fun _ -> CTD.empty
                                |> CTD.insert (string_to_list "test") 5
                                |> CTD.insert (string_to_list "test") 5
                                |> CTD.get (string_to_list "test")
                                |> assert_equal (Some 5));
  "insert replace " >:: (fun _ -> CTD.empty
                                  |> CTD.insert (string_to_list "test") 5
                                  |> CTD.insert (string_to_list "test") 20
                                  |> CTD.get (string_to_list "test")
                                  |> assert_equal (Some 20));
  "insert sub get sub" >:: (fun _ -> CTD.empty
                                     |> CTD.insert (string_to_list "test") 5
                                     |> CTD.insert (string_to_list "tes") 20
                                     |> CTD.get (string_to_list "tes")
                                     |> assert_equal (Some 20));
  "insert sub get sub other order" >:: (fun _->
      CTD.empty
      |> CTD.insert (string_to_list "tes") 5
      |> CTD.insert (string_to_list "test") 20
      |> CTD.get (string_to_list "tes")
      |> assert_equal (Some 5));
  "insert sub get super" >:: (fun _ -> CTD.empty
                                       |> CTD.insert (string_to_list "test") 5
                                       |> CTD.insert (string_to_list "tes") 20
                                       |> CTD.get (string_to_list "test")
                                       |> assert_equal (Some 5));
  "complex get hello" >:: (fun _ -> complex_trie
                                    |> CTD.get (string_to_list "hello")
                                    |> assert_equal (Some 10));
  "complex get hi" >:: (fun _ -> complex_trie
                                 |> CTD.get (string_to_list "hi")
                                 |> assert_equal (Some 20));
  "complex get test" >:: (fun _ -> complex_trie
                                   |> CTD.get (string_to_list "test")
                                   |> assert_equal (Some 3));
  "complex get testing" >:: (fun _ -> complex_trie
                                      |> CTD.get (string_to_list "testing")
                                      |> assert_equal (Some 4));
  "complex get beep" >:: (fun _ -> complex_trie
                                   |> CTD.get (string_to_list "beep")
                                   |> assert_equal (Some 5));
  "complex get jack" >:: (fun _ -> complex_trie
                                   |> CTD.get (string_to_list "jack")
                                   |> assert_equal (Some 30));
  "complex get tony" >:: (fun _ -> complex_trie
                                   |> CTD.get (string_to_list "tony")
                                   |> assert_equal (Some 40));
  "complex get chris" >:: (fun _ -> complex_trie
                                    |> CTD.get (string_to_list "chris")
                                    |> assert_equal (Some 50));
]

let suite = "trie test suite" >::: trie_tests

let _ = run_test_tt_main suite