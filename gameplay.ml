type t = {
  board: Board.t;
  checker: WordChecker.t
}

module CM = StandardCompletedMove.StandardCompletedMove

let make_gameplay b c = {board = b; checker = c}

let is_inside (in_x, in_y) (out_x, out_y) =
  in_x >= 0 && in_x < out_x && in_y >= 0 && in_y < out_y

let next_move move =
  let direction = ProposedMove.direction move in
  let (x, y) = ProposedMove.location move in
  let loc = match direction with
    | Across -> (x + 1, y)
    | Down -> (x, y + 1)
  in
  ProposedMove.create direction loc (move |>  ProposedMove.letters |> List.tl)

let rec update_board move t =
  match ProposedMove.letters move with
  | [] -> Some t
  | h::tail -> let loc = ProposedMove.location move in
    if is_inside loc (Board.size t.board)
    && Board.query_tile (fst loc) (snd loc) t.board = None then
      update_board (move |> next_move) {
        checker = t.checker;
        board = Board.set_tile (fst loc) (snd loc) h t.board;
      }
    else
      None

let execute move t = failwith "unimplemented"

let query_tile x y t = Board.query_tile x y t.board

let obtain_board t = 
  t.board

