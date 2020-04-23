open Board
open TrieDictionary
open TileInventory
open Player
open Gameplay
open State

let rec main () = 
  ANSITerminal.(print_string [red; Bold] (" " ^
                                          "\n\n Welcome to OScrabble, a functional implementaiton of Scrabble.\n"))

let () = main ()