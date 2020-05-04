open Board

type t = {
  board: Board.t;
  checker: WordChecker.t;
  score: int;
}

module CM = StandardCompletedMove.StandardCompletedMove

let make_gameplay b c = { board = b; checker = c; score = 0; }

exception InvalidWord of string

let total_score t : int option = try
    let width = List.init (t.board |> Board.size |> fst) (fun i -> i) in
    let height = List.init (t.board |> Board.size |> snd) (fun i -> i) in
    let line_sum d1 d2 get =
      d1 |> List.iter
        (fun d1' ->
           d2
           |> List.map (fun d2' -> get d1' d2')
           |> List.filter_map (fun o -> o)
           |> List.map (function
               | Filled c -> c
               | _ -> ' ')
           |> List.map (String.make 1)
           |> String.concat ""
           |> String.split_on_char ' '
           |> List.filter (fun s -> String.length s > 1)
           |> List.iter (fun word -> print_endline ("checking :" ^ word ^ ";"); if WordChecker.check word t.checker
                          then ()
                          else raise (InvalidWord word)
                        )
        ) in
    line_sum width height (fun d1' d2' -> Board.query_tile d2' d1' t.board);
    line_sum height width (fun d1' d2' -> Board.query_tile d1' d2' t.board);
    Some (width
          |> List.fold_left
            (fun (acc:int) (c:int) ->
               acc + (height
                      |> List.map (fun (r:int) -> Board.query_tile r c t.board)
                      |> List.filter_map (function
                          | Some (Filled c) -> Some c
                          | _ -> None)
                      |> List.fold_left (fun acc c -> acc + (CM.LV.get c)) 0
                     )
            ) 0
         )
  with InvalidWord word -> print_endline ("bad word " ^ word); None

let is_inside (in_x, in_y) (out_x, out_y) =
  in_x >= 0 && in_x < out_x && in_y >= 0 && in_y < out_y

let is_filled = function
  | Some (Bonus _)
  | Some (Empty) -> false
  | _ -> true

let next_move move =
  let direction = ProposedMove.direction move in
  let (x, y) = ProposedMove.location move in
  let loc = match direction with
    | Across -> (x + 1, y)
    | Down -> (x, y + 1)
  in
  ProposedMove.create direction loc (move |>  ProposedMove.letters |> List.tl)

let rec update_board move chars t =
  match ProposedMove.letters move with
  | [] -> Some (t, chars)
  | h::tail -> let loc = ProposedMove.location move in
    if is_inside loc (Board.size t.board)
    && (Board.query_tile (snd loc) (fst loc) t.board |> is_filled |> not) then begin
      let bonus = Board.check_bonus (snd loc) (fst loc) in
      update_board (move |> next_move) ((h, bonus)::chars) {
        t with
        checker = t.checker;
        board = Board.set_tile (snd loc) (fst loc) h t.board;
      }
    end
    else begin
      None
    end

let execute move t =
  Option.bind (update_board move [] t)
    (fun ((nt:t), _)
      -> total_score nt
         |> Option.map (fun (s: int) -> ({
             nt with score = s
           }, s - nt.score))
    )

let query_tile x y t = Board.query_tile x y t.board

let obtain_board t = 
  t.board

