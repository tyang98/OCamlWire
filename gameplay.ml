open Board

type t = {
  board: Board.t;
  checker: WordChecker.t;
}

module ScrabblePoint = struct 
  let get char =
    let point_values =
      [('A', 1); ('B', 3); ('C', 3); ('D', 2); ('E', 1); ('F', 4);
       ('G', 2); ('H', 4); ('I', 1); ('J', 8); ('K', 5); ('L', 1);
       ('M', 3); ('N', 1); ('O', 1); ('P', 3); ('Q', 10); ('R', 1);
       ('S', 1); ('T', 1); ('U', 1); ('V', 4); ('W', 4); ('X', 8);
       ('Y', 4); ('Z', 8)] in
    let upper_char = Char.uppercase_ascii char in
    List.assoc upper_char point_values
end

let make_gameplay b c = { board = b; checker = c; }

exception InvalidWord of string
exception Spaces

type c = New of char * bonus option | Old of char

(** [word_score word] is the score of a just-played [word], accounting for
    bonuses associated with newly added tiles. *)
let word_score word =
  let rec loop acc wbs = function
    | h::t -> begin match h with
        | New (c, Some (WordBonus x)) -> 
          loop (ScrabblePoint.get c + acc) (x * wbs) t
        | New (c, Some (LetterBonus x)) ->
          loop (ScrabblePoint.get c * x + acc) wbs t
        | New (c, _)
        | Old c -> loop (ScrabblePoint.get c + acc) wbs t
      end
    | [] -> wbs * acc
  in
  loop 0 1 word

(** [score added t] is the point value of a just-completed move, where [t] is
    the updated state and [added] is an association list from positions to
    characters and bonuses of the newly added tiles. *)
let score (added: ((int * int) * (char * bonus option)) list) t : int option =
  try
    let (w, h) = t.board |> Board.size in
    let width = List.init w (fun i -> w - 1 - i) in
    let height = List.init h (fun i -> h - 1 - i) in
    let line_sum d1 d2 get =
      d1 |> List.fold_left
        (fun acc d1' ->
           acc + (d2 |> List.map (fun d2' -> get d1' d2')
                  |> List.fold_left (fun acc c -> match c with
                      | Some c-> (c::(List.hd acc))::(List.tl acc)
                      | None -> []::acc) [[]]
                  |> List.filter (fun w ->
                      List.length w > 1 && 
                      w |> List.find_opt (fun c ->
                          match c with New _ -> true | _ -> false)
                      |> Option.is_some)
                  |> (fun l -> if List.length l > 1 then raise Spaces else l)
                  |> List.map (fun word ->
                      let str_word = word |> List.map (fun c -> match c with
                          | New (c, _)
                          | Old c -> String.make 1 c)
                                     |> String.concat ""
                      in
                      if WordChecker.check str_word t.checker
                      then word_score word
                      else raise (InvalidWord str_word)) 
                  |> List.fold_left (+) 0)) 0 in
    Some (line_sum width height (fun d1' d2' ->
        match List.assoc_opt (d2', d1') added with
        | Some (c, b) -> Some (New (c, b))
        | None -> match Board.query_tile d2' d1' t.board with
          | Some (Filled c) -> Some (Old c)
          | _ -> None) + line_sum height width (fun d1' d2' ->
        match List.assoc_opt (d1', d2') added with
        | Some (c, b) -> Some (New (c, b))
        | None -> match Board.query_tile d1' d2' t.board with
          | Some (Filled c) -> Some (Old c)
          | _ -> None))
  with InvalidWord _ | Spaces -> None

(** [is_inside (in_x, in_y) (out_x, out_y)] is  whether or not [in_x, in_y] is
    inside of of [out_x, out_y]. *)
let is_inside (in_x, in_y) (out_x, out_y) =
  in_x >= 0 && in_x < out_x && in_y >= 0 && in_y < out_y

(** [is_filled opt] is true if a tile has already been 
    filled, false if the [tile option] contains either a Bonus or 
    Empty tile. *)
let is_filled = function
  | Some (Bonus _)
  | Some (Empty) -> false
  | _ -> true

(** [next_move move] is the next ProposedMove after one character is placed 
    from [move]. *)
let next_move move =
  let direction = ProposedMove.direction move in
  let (x, y) = ProposedMove.location move in
  let loc = match direction with
    | Across -> (x + 1, y)
    | Down -> (x, y + 1)
  in
  ProposedMove.create direction loc (move |>  ProposedMove.letters |> List.tl)

(** [prebious_loc move] is the location ([(x, y)]) immediately before the
    starting location of [move]. *)
let previous_loc move =
  let direction = ProposedMove.direction move in
  let (x, y) = ProposedMove.location move in
  match direction with
  | Across -> (x - 1, y)
  | Down -> (x, y - 1)

(** [filled_space x y t] is whether the specified location contains a letter 
    in [t]. *)
let filled_space x y t =
  is_inside (x, y) (Board.size t.board)
  && Board.query_tile y x t.board |> is_filled

(** [conencted (x, y) d t] is whether there are any letters on either side of
    the given position. *)
let connected (x, y) (d:ProposedMove.direction) t =
  (Board.check_bonus y x t.board |> function Some Start -> true | _ -> false)
  || (let ((x1, y1), (x2, y2)) = match d with
      | Across -> ((x, y + 1), (x, y - 1))
      | Down -> ((x + 1, y), (x - 1, y))
     in
     filled_space x1 y1 t || filled_space x2 y2 t)

(** [update_board move chars t] updates the current gameplay board with 
    the ProposedMove [move]. *)
let update_board move t =
  let rec inner move chars connections t =
    match ProposedMove.letters move with
    | [] -> if List.exists (fun b -> b) connections
            || let (x, y) = ProposedMove.location move in filled_space x y t
      then Some (t, chars) else None
    | h::tail -> let loc = ProposedMove.location move in
      if is_inside loc (Board.size t.board)
      && (Board.query_tile (snd loc) (fst loc) t.board |> is_filled |> not) 
      then let bonus = Board.check_bonus (snd loc) (fst loc) t.board in
        inner (move |> next_move) ((((snd loc), (fst loc)), (h, bonus))::chars)
          ((connected loc (ProposedMove.direction move) t)::connections) {
          checker = t.checker;
          board = Board.set_tile (snd loc) (fst loc) h t.board;
        }
      else None
  in
  inner move [] [let (x, y) = previous_loc move in filled_space x y t] t

let execute moves t =
  let rec update moves t = match moves with
    | [] -> Some (t, [])
    | m::s -> Option.bind (update_board m t)
                (fun (nt, chars) ->
                   Option.map (fun (nnt, l) -> nnt, chars @ l) (update s nt))
  in
  Option.bind (update moves t)
    (fun ((nt:t), chars)
      -> score chars nt
         |> Option.map (fun (s: int) -> nt, s)
    )

let query_tile x y t = Board.query_tile x y t.board

let obtain_board t = 
  t.board