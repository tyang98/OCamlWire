
type bonus = WordBonus of int | LetterBonus of int * char

type tile = {
  x_coord: int;
  y_coord: int;
  filled : char;
  isBonus : bonus option;
}

type t = tile list

let query_tile r c b = failwith "Unimplemented"

let check_bonus r c b = failwith "Unimplemented"

let set_tile r c l b = failwith "Unimplemented"

let size b = 
  List.length b
