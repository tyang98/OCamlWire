open Gameplay

type tile = TileInventory.tile

type t = (int) * (tile list)

let new_p = (0, [])

let update_tile t p = (fst p, t)

let add_tile le p = p |> fun (moves, tiles) -> (moves, le::tiles)

let add_score amount p = p |> fun (score, tiles) -> (score + amount, tiles)

let score p = fst p

let tiles p = snd p