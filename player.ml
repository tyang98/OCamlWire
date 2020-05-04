open Gameplay

module CM = StandardCompletedMove.StandardCompletedMove

type tile = Letter of char | Blank


type t = (CM.t list) * (tile list)

let new_p = ([], [])

let add_tile le p = p |> fun (moves, tiles) -> (moves, le::tiles)

let add_move move p = p |> fun (moves, tiles) -> (move::moves, tiles)

let score p = List.fold_left (fun a b -> CM.score b + a) 0 (fst p)

let tiles p = snd p