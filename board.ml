
type bonus = WordBonus of int | LetterBonus of int | Start 

type tile = Filled of char | Bonus of bonus | Empty 

type t = (tile list) list

let board (t:t) : tile list list = 
  t

(** [bonus_extract bonus r c] is the tile of type Empty or Bonus located at
    row [r] and column [c]. *)
let bonus_extract bonus (r : int) (c : int) =
  match bonus |> List.filter (fun (a, b, _) -> r = a && c = b) with
  | [] -> Empty
  | (_,_,c)::_ -> Bonus c

(** [fill_row bonuses size row l] is a list of tiles which are either empty or
    filled with bonuses based on the supplied bonuses list, and the [row] 
    supplied. *)
let rec fill_row bonuses (size : int) (row : int) (l : tile list) : tile list =
  match size with 
  | 0 -> l
  | _ -> fill_row bonuses (size - 1) row 
           ((bonus_extract bonuses (row - 1) (size - 1))::l)

(** [recurse_out bonuses size l] is a list of tile lists, each of which are
    filled with either empty spaces or bonuses. *)
let rec recurse_out bonuses (total: int) (size : int) 
    (l : tile list list) : tile list list =
  match size with 
  | 0 -> l
  | _ -> recurse_out bonuses total (size - 1) 
           ((fill_row bonuses total size [])::l)

let rec init_board bonuses size = recurse_out bonuses size size []

let query_tile r c b =  
  match List.nth_opt b r with 
  | Some p -> List.nth_opt p c
  | None -> failwith "Invalid board shape"

let check_bonus r c b = 
  match query_tile r c b with 
  | Some x -> 
    begin
      match x with 
      | Filled c -> None
      | Empty -> None
      | Bonus b -> Some b
    end
  | None -> None

let set_tile r c l b = 
  List.mapi (fun addr outer_list -> if addr = r 
              then List.mapi (fun addr ch -> if addr = c then Filled l
                               else ch) outer_list
              else outer_list) b

let size b = 
  (List.length b, 
   List.nth_opt b 0 
   |> (function Some r -> r |> List.length | None -> failwith "Illegal board")
  )


