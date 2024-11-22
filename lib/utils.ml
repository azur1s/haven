let todo ?(reason="") loc =
  print_endline @@ loc ^ ". Reason:";
  if reason != "" then
    print_endline reason;
  raise (Failure "TODO")

let ( let* ) x f =
  match x with
  | Ok v -> f v
  | Error e -> Error e

let ( @* ) list i = List.nth list i

(* 0 -> a, 1 -> b ...26 -> z, 27 -> a1, 28 -> b1 ... *)
let str_from_int i =
  if i < 26 then String.make 1 (Char.chr (i + 97))
  else String.make 1 (Char.chr (i mod 26 + 97)) ^ string_of_int (i / 26)

let capitalize_first s =
  match String.length s with
  | 0 -> ""
  | _ -> String.capitalize_ascii (String.make 1 s.[0]) ^ String.sub s 1 (String.length s - 1)

let rec map_early_return f = function
  | [] -> Ok []
  | x :: xs ->
    (match f x with
    | Error e -> Error e
    | Ok y -> (
        match map_early_return f xs with
        | Error e -> Error e
        | Ok ys -> Ok (y :: ys)))

let map_sep_results xs =
  List.partition_map (function Ok v -> Left v | Error e -> Right e) xs
