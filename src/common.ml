(* Common types used by representations *)
open Loc
open Sexplib0

type err =
  { msg : string
  ; hint : string option
  ; loc : span
  }
  [@@deriving show]

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

let from_result = function
| Ok x -> x
| Error (msg, loc) -> err msg loc

let sexp_of_bool   b = Sexp.Atom (string_of_bool b)
let sexp_of_int    i = Sexp.Atom (string_of_int i)
let sexp_of_float  f = Sexp.Atom (string_of_float f)
let sexp_of_string s = Sexp.Atom s
let sexp_of_list f l = Sexp.List (List.map f l)

type lit =
  | LUnit
  | LBool  of bool
  | LInt   of int
  | LFloat of float
  | LStr   of string
  | LSym   of string
  [@@deriving show, sexp_of]

and pattern =
  | PatLit of lit
  [@@deriving show, sexp_of]

and bin =
  | Add | Sub | Mul | Div | Mod
  | Eq  | Neq | Lt  | Lte | Gt | Gte
  | And | Or
  | Cons
  [@@deriving show, sexp_of]

and typ =
  | TyConst of string
  | TyTuple of typ * typ
  | TyArrow of typ * typ
  | TyConstructor of string * typ
  | TyRecord of (string * typ) list
  | TyInfer of string
  [@@deriving show, sexp_of]

let string_of_lit = function
  | LUnit -> "null"
  | LBool b -> string_of_bool b
  | LInt i -> string_of_int i
  | LFloat f -> string_of_float f
  | LStr s -> "\"" ^ s ^ "\""
  | LSym s -> s

let string_of_bin = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "rem"
  | Eq -> "=="
  | Neq -> "/="
  | Lt -> "<"
  | Lte -> "<="
  | Gt -> ">"
  | Gte -> ">="
  (* Short-circuiting *)
  | And -> "andalso"
  | Or -> "orelse"
  | Cons -> "|"

let rec string_of_typ = function
  | TyConst s -> s
  | TyTuple (a, b) -> string_of_typ a ^ " * " ^ string_of_typ b
  | TyArrow (a, b) -> string_of_typ a ^ " -> " ^ string_of_typ b
  | TyRecord l -> "{" ^ String.concat ", " (List.map (fun (f, t) -> f ^ " : " ^ string_of_typ t) l) ^ "}"
  | TyConstructor (f, b) -> string_of_typ b ^ " " ^ f
  | TyInfer s -> s
