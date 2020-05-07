module type LetterValueMap = sig 
  val get : char -> int
end 

module type CompletedMove = sig
  type t

  module LV : LetterValueMap

  val score : t -> int
  val words : t -> string list
  val from : (string * (char * int) list * int list) list -> t
end

(** [split s] is the list of characters formed from each individual
    letter in string [s]. *)
let split (s : string) : char list = 
  List.init (String.length s) (String.get s) 

module Make = 
  functor (LetterVal : LetterValueMap) -> struct
    type t = (string * (char * int) list * int list) list

    module LV = LetterVal

    let score m = 
      let rec add_bonus_letters (bonus : (char * int) list) 
          (ls : (char * int) list) : (char * int) list = 
        match bonus with
        | (l, b)::t -> begin 
            (* Match along the letters in the words to 
               see if it contains said letter *)
            match List.assoc_opt l ls with 
            | Some r -> add_bonus_letters t ((l, r * (b - 1))::ls)
            | _ -> add_bonus_letters t ls
          end
        | [] -> ls
      in
      List.fold_left (+) 0 
        ((* first map each string to a list of characters *)
          m |> List.map (fun (str, lb, wb) -> (split str, lb, wb))
          |> List.map (
            fun (chrs, lb, wb) -> 
              (* For each word, map each letter to its associated point value,
                 then sum them *)
              (List.map (fun x -> (x, LV.get x)) chrs 
               |> add_bonus_letters lb
               |> List.fold_left (fun x y -> x + snd y) 0, wb ))
          |> List.map 
            (fun (i, wb) -> i * (List.fold_left (fun a b -> a * b) 1 wb)))

    let words m = List.map (fun (str, _, _) -> str) m

    let from lst = lst
  end