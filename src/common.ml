(* Reporting *)

type span =
  { file: string
  (* line, column *)
  ; pos_start: int * int
  ; pos_end: int * int
  } [@@deriving show]

type 'a spanned = 'a * span
  [@@deriving show]

let string_of_span s =
  Printf.sprintf "%s line %d characters %d-%d" s.file
    (fst s.pos_start) (snd s.pos_start)
    (* (fst s.pos_end) *) (snd s.pos_end)

let span_union s1 s2 =
  { file = s1.file
  ; pos_start = (min (fst s1.pos_start) (fst s2.pos_start),
                 min (snd s1.pos_start) (snd s2.pos_start))
  ; pos_end = (max (fst s1.pos_end) (fst s2.pos_end),
               max (snd s1.pos_end) (snd s2.pos_end))
  }

type err =
  { msg: string
  ; hint: string option
  ; loc: span
  } [@@deriving show]

let err ?(hint="") msg loc =
  { msg; hint = if hint = "" then None else Some hint; loc }

let with_hint hint err =
  { err with hint = Some hint }

let err_ret ?(hint="") msg loc =
  let e = if hint = "" then
    err msg loc
  else
    err msg loc |> with_hint hint
  in Error e

(* Common representations *)

type value =
  | VUnit
  | VBool of bool
  | VInt of int
  | VStr of string
  [@@deriving show]

let string_of_value = function
  | VUnit   -> "unit"
  | VBool b -> string_of_bool b
  | VInt i  -> string_of_int i
  | VStr s  -> Printf.sprintf "\"%s\"" s

type bin =
  | Mul | Div | Mod
  | Add | Sub
  | Eq | Neq
  | Lt | Le | Gt | Ge
  | And | Or
  [@@deriving show]

let string_of_bin = function
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Add -> "+"
  | Sub -> "-"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | And -> "&&"
  | Or -> "||"

(* Utilities *)

let ( let* ) x f =
  match x with
  | Ok v -> f v
  | Error e -> Error e

let combine_results xs =
  List.fold_right (fun r acc ->
    match (r, acc) with
    | (Ok v, Ok vs) -> Ok (v :: vs)
    | (Error e, Ok _) -> Error e
    | (Ok _, Error e) -> Error e
    | (Error e1, Error e2) -> Error (e1 @ e2)
  ) xs (Ok [])

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
