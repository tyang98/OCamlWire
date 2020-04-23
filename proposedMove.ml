type direction = Across | Down

type t = {
  letter : char list;
  current_loc : (int * int);
  direct : direction;
}

let create dir loc l = {
  letter = l;
  direct = dir;
  current_loc = loc;
}

let letters m = 
  m.letter

let location m = 
  m.current_loc

let direction m = 
  m.direct