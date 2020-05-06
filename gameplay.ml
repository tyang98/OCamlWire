open Board

type t = {
  board: Board.t;
  checker: WordChecker.t;
}

module CM = StandardCompletedMove.StandardCompletedMove

(** [make_gameplay b c] is the new gameplay given Board [b] and 
    WordChecker [c]. *)
let make_gameplay b c = { board = b; checker = c; }

exception InvalidWord of string

type c = New of char * bonus option | Old of char

let word_score word =
  let rec loop acc wbs = function
    | h::t -> begin match h with
        | New (c, Some (WordBonus x)) -> loop (CM.LV.get c + acc) (x * wbs) t
        | New (c, Some (LetterBonus x)) -> loop (CM.LV.get c * x + acc) wbs t
        | New (c, _)
        | Old c -> loop (CM.LV.get c + acc) wbs t
      end
    | [] -> wbs * acc
  in
  loop 0 1 word

let score (added: ((int * int) * (char * bonus option)) list) t : int option =
  try
    let (w, h) = t.board |> Board.size in
    let width = List.init w (fun i -> w - 1 - i) in
    let height = List.init h (fun i -> h - 1 - i) in
    let line_sum d1 d2 get =
      d1 |> List.fold_left
        (fun acc d1' ->
           acc + (d2
                  |> List.map (fun d2' -> get d1' d2')
                  |> List.fold_left (fun acc c -> match c with
                      | Some c-> (c::(List.hd acc))::(List.tl acc)
                      | None -> []::acc) [[]]
                  |> List.filter (fun w ->
                      List.length w > 1 &&
                      w
                      |> List.find_opt (fun c ->
                          match c with New _ -> true | _ -> false)
                      |> Option.is_some)
                  |> List.map (fun word ->
                      let str_word = word |> List.map (fun c -> match c with
                          | New (c, _)
                          | Old c -> String.make 1 c)
                                     |> String.concat ""
                      in
                      print_endline ("checking :" ^ str_word ^ ";");
                      if WordChecker.check str_word t.checker
                      then word_score word
                      else raise (InvalidWord str_word)
                    )
                  |> List.fold_left (+) 0
                 )
        ) 0 in
    Some (
      line_sum width height (fun d1' d2' ->
          match List.assoc_opt (d2', d1') added with
          | Some (c, b) -> Some (New (c, b))
          | None -> match Board.query_tile d2' d1' t.board with
            | Some (Filled c) -> Some (Old c)
            | _ -> None)
      + line_sum height width (fun d1' d2' ->
          match List.assoc_opt (d1', d2') added with
          | Some (c, b) -> Some (New (c, b))
          | None -> match Board.query_tile d1' d2' t.board with
            | Some (Filled c) -> Some (Old c)
            | _ -> None)
    )
  with InvalidWord word -> print_endline ("bad word " ^ word); None

(** TODO: Document *)
let is_inside (in_x, in_y) (out_x, out_y) =
  in_x >= 0 && in_x < out_x && in_y >= 0 && in_y < out_y

(** [is_filled opt] is the boolean that is true if a tile has already been 
    filled, false if [opt] either a Bonus or Empty tile. *)
let is_filled = function
  | Some (Bonus _)
  | Some (Empty) -> false
  | _ -> true

(** [next_move move] is the next ProposedMove depending on the direction 
    and location specified in the current ProposedMove [move]. *)
let next_move move =
  let direction = ProposedMove.direction move in
  let (x, y) = ProposedMove.location move in
  let loc = match direction with
    | Across -> (x + 1, y)
    | Down -> (x, y + 1)
  in
  ProposedMove.create direction loc (move |>  ProposedMove.letters |> List.tl)

(** TODO: Document  *)
let rec update_board move chars t =
  match ProposedMove.letters move with
  | [] -> Some (t, chars)
  | h::tail -> let loc = ProposedMove.location move in
    if is_inside loc (Board.size t.board)
    && (Board.query_tile (snd loc) (fst loc) t.board |> is_filled |> not) then
      let bonus = Board.check_bonus (snd loc) (fst loc) t.board in
      update_board (move |> next_move)
        ((((snd loc), (fst loc)), (h, bonus))::chars) {
        checker = t.checker;
        board = Board.set_tile (snd loc) (fst loc) h t.board;
      }
    else None


let execute move t =
  Option.bind (update_board move [] t)
    (fun ((nt:t), chars)
      -> score chars nt
         |> Option.map (fun (s: int) -> nt, s)
    )

let query_tile x y t = Board.query_tile x y t.board

let obtain_board t = 
  t.board