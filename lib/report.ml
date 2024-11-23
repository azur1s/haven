open Loc

let loc_to_line_col str loc =
  let lines = String.split_on_char '\n' str in
  let rec loop i acc = function
    | [] -> failwith "loc_to_line_col: invalid loc"
    | hd :: tl ->
      if i + String.length hd >= loc.start then
        (acc, loc.start - i)
      else
        loop (i + String.length hd + 1) (acc + 1) tl
  in
  loop 0 1 lines

let get_line str idx =
  String.split_on_char '\n' str
  |> fun x -> List.nth x idx

let get_line_opt str idx =
  try Some (get_line str idx)
  with _ -> None

let left_pad str len =
  let str_len = String.length str in
  if str_len >= len then
    str
  else
    String.make (len - str_len) ' ' ^ str

let report path contents m loc =
  let (line, col) = loc_to_line_col contents loc in

  let prefix i = " " ^ left_pad (string_of_int i) 4 ^ "   " in
  let curr_line = get_line_opt contents (line - 1)
    |> Option.map (fun x -> prefix (line - 1) ^ x)
  in
  let prev_line = get_line_opt contents (line - 2)
    |> Option.map (fun x -> prefix (line - 2) ^ x)
  in

  [ path ^ ":" ^ string_of_int line ^ ":" ^ string_of_int (col + 1)
  ; "" ] @

  ([ prev_line
  ; curr_line
  ]
  |> List.filter_map Fun.id) @

  [ String.make (col + String.length (prefix (line - 1))) ' ' ^ "^"
  ; ""
  ; m
  ]
  |> String.concat "\n"
  |> print_endline