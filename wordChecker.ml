open TrieDictionary

module Dict = TrieDictionary.Make(struct
    module Target = Char

    type t = string

    let to_list s = List.init (String.length s) (String.get s)
  end)

type t = unit Dict.t


let check word t = Dict.check word t

let get_line file = try Some (input_line file) with End_of_file -> None

let load_from_file path =
  let file = open_in path in
  let rec loop acc =
    match get_line file with
    | Some line when not (0 = String.length line) ->
      Dict.insert (String.lowercase_ascii line) () acc |> loop
    | _ -> acc
  in
  let dict = loop Dict.empty in
  close_in file;
  dict