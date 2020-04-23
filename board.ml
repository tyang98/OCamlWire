
type bonus = WordBonus of int | LetterBonus of int * char

type tile = Filled of char | Bonus of bonus | Empty

type t = (tile list) list

(** [bonus_extract bonus r c] is the tile of type Empty or Bonus located at
    row [r] and column [c] *)
let bonus_extract bonus (r : int) (c : int) =
  match bonus |> List.filter (fun (a, b, _) -> r = a && c = b) with
  | [] -> Empty
  | (_,_,c)::t -> Bonus c

(** [tile_list_help bonuses r c list] is the list of tiles with *)
let rec tile_list_help (bonuses : (int * int * bonus) list) 
    (r : int) (c : int) (list : tile list) : tile list =
  match c with 
  | 0 -> list
  | _ -> (bonus_extract bonuses r c)::(tile_list_help bonuses r (c - 1) list)

let rec init_board_help (bonuses : (int * int * bonus) list) (size_r : int) 
    (size_c : int) (list : tile list list) : tile list list = 
  match size_r with 
  | 0 -> list 
  | r -> (List.rev (tile_list_help bonuses r size_c []))
         ::(init_board_help bonuses (r - 1) size_c list)

let rec init_board bonuses size = 
  init_board_help bonuses size size [] |> List.rev


(* Board Config:
   Double Letter Score:
   (1,4)  (3,7) (4,1)  (7,3) (8,4)  (9,3) (12,1) (13,7) (15,4) 
   (1,12) (3,9) (4,8)  (7,7) (8,12) (9,7) (12,8) (13,9) (15,12)
                (4,15) (7,9)        (9,9) (12,15)
                       (7,13)       (9,13)
   Double Word Score:
   (2,2)  (3,3)  (4,4)  (5,5)  (11,5)  (12,4)  (13,3)  (14,2)
   (2,14) (3,13) (4,12) (5,11) (11,11) (12,12) (13,13) (14,14) 

   Triple Letter Score:
   (2,6)   (6,2)  (10,2)  (14,6)
   (2,10)  (6,6)  (10,6)  (14,10)
          (6,10) (10,10)
          (6,14) (10,14) 
   Triple Word Score:
   (1,1)  (8,1)  (15,1)
   (1,8)  (8,15) (15,8)
   (1,15)        (15,15)
*)

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
              then List.mapi (fun addr inner_list -> if addr = c then Filled l
                               else inner_list) outer_list
              else outer_list) b

let size b = 
  (List.length b, 
   List.nth_opt b 0 
   |> (function Some r -> r |> List.length | None -> failwith "Illegal board")
  )


