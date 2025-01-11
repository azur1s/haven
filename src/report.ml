open Common
open Loc

let red    = "\027[31m"
let green  = "\027[32m"
let yellow = "\027[33m"
let blue   = "\027[34m"
let purple = "\027[35m"
let cyan   = "\027[36m"

let dim = "\027[2m"
let reset = "\027[0m"

let highlight_kw s =
  let color = match s with
    | "let" | "rec" | "fun" -> cyan
    | "if" | "then" | "else" | "use" -> purple
    | "()" -> yellow
    | _ -> ""
  in
  color ^ s ^ reset

let highlight s =
  String.split_on_char ' ' s
  |> List.map highlight_kw
  |> String.concat " "

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

let report path contents err =
  let (line, col) = loc_to_line_col contents err.loc in

  let prefix i = " " ^ left_pad (string_of_int i) 4 ^ "   " in
  let curr_line = get_line_opt contents (line - 1)
    |> Option.map (fun x -> dim ^ prefix (line - 1) ^ reset ^ highlight x)
  in
  let prev_line = get_line_opt contents (line - 2)
    |> Option.map (fun x -> dim ^ prefix (line - 2) ^ reset ^ highlight x)
  in

  let xs =
    [ ""
    ; red ^ "ERR " ^ reset ^ path ^ ":" ^ string_of_int line ^ ":" ^ string_of_int (col + 1)
    ; "" ] in
  let xs = xs @
    ([ prev_line
    ; curr_line
    ]
    |> List.filter_map Fun.id) in
  let xs = xs @
    [ red ^ String.make (col + String.length (prefix (line - 1))) ' ' ^ "^" ^ reset
    ; ""
    ; err.msg
    ] in
  let xs = if Option.is_some err.hint then
    xs @ [ blue ^ "HINT " ^ reset ^ Option.get err.hint ^ reset ]
  else
    xs in
  xs
  |> String.concat "\n"
  |> print_endline