type tile = TileInventory.tile

type t = tile list

let create lst = List.map (fun str -> str.[0] |> TileInventory.tile_of_char) lst 

let tiles t = t

let size s = List.length s 