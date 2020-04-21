open OUnit2
open TrieDictionary

let string_to_list s = List.init (String.length s) (String.get s)

module CTD = TrieDictionary.Make(struct
    module Target = Char

    type t = string

    let to_list = string_to_list
  end)

let complex_trie = CTD.empty
                   |> CTD.insert ("hello") 1
                   |> CTD.insert ( "hi") 2
                   |> CTD.insert ( "test") 3
                   |> CTD.insert ( "testing") 4
                   |> CTD.insert ( "beep") 5
                   |> CTD.insert ( "hello") 10
                   |> CTD.insert ( "hi") 20
                   |> CTD.insert ( "jack") 30
                   |> CTD.insert ( "tony") 40
                   |> CTD.insert ( "chris") 50

let trie_tests = [
  "insert 1" >:: (fun _ -> CTD.empty |> CTD.insert ( "test") 5
                           |> CTD.get ( "test")
                           |> assert_equal (Some 5));
  "insert twice " >:: (fun _ -> CTD.empty
                                |> CTD.insert ( "test") 5
                                |> CTD.insert ( "test") 5
                                |> CTD.get ( "test")
                                |> assert_equal (Some 5));
  "insert replace " >:: (fun _ -> CTD.empty
                                  |> CTD.insert ( "test") 5
                                  |> CTD.insert ( "test") 20
                                  |> CTD.get ( "test")
                                  |> assert_equal (Some 20));
  "insert sub get sub" >:: (fun _ -> CTD.empty
                                     |> CTD.insert ( "test") 5
                                     |> CTD.insert ( "tes") 20
                                     |> CTD.get ( "tes")
                                     |> assert_equal (Some 20));
  "insert sub get sub other order" >:: (fun _->
      CTD.empty
      |> CTD.insert ( "tes") 5
      |> CTD.insert ( "test") 20
      |> CTD.get ( "tes")
      |> assert_equal (Some 5));
  "insert sub get super" >:: (fun _ -> CTD.empty
                                       |> CTD.insert ( "test") 5
                                       |> CTD.insert ( "tes") 20
                                       |> CTD.get ( "test")
                                       |> assert_equal (Some 5));
  "complex get hello" >:: (fun _ -> complex_trie
                                    |> CTD.get ( "hello")
                                    |> assert_equal (Some 10));
  "complex get hi" >:: (fun _ -> complex_trie
                                 |> CTD.get ( "hi")
                                 |> assert_equal (Some 20));
  "complex get test" >:: (fun _ -> complex_trie
                                   |> CTD.get ( "test")
                                   |> assert_equal (Some 3));
  "complex get testing" >:: (fun _ -> complex_trie
                                      |> CTD.get ( "testing")
                                      |> assert_equal (Some 4));
  "complex get beep" >:: (fun _ -> complex_trie
                                   |> CTD.get ( "beep")
                                   |> assert_equal (Some 5));
  "complex get jack" >:: (fun _ -> complex_trie
                                   |> CTD.get ( "jack")
                                   |> assert_equal (Some 30));
  "complex get tony" >:: (fun _ -> complex_trie
                                   |> CTD.get ( "tony")
                                   |> assert_equal (Some 40));
  "complex get chris" >:: (fun _ -> complex_trie
                                    |> CTD.get ( "chris")
                                    |> assert_equal (Some 50));
]

let suite = "trie test suite" >::: trie_tests

let _ = run_test_tt_main suite