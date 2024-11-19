let todo ?(reason="") loc =
  failwith @@ "TODO" ^ (if reason != "" then " " ^ reason else "") ^ " " ^ loc

let unreachable loc = failwith @@ "Unreachable " ^ loc

let ( let* ) x f =
  match x with
  | Ok v -> f v
  | Error e -> Error e

let ( @ ) list i = List.nth list i

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
