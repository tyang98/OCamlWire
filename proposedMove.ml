type direction = Across | Down

type t = {
  letter : char list;
  current_loc : (int * int);
  direct : direction;
}

let letters m = 
  m.letter

let location m = 
  m.current_loc

let direction m = 
  m.direct