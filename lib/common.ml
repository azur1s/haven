(* Common types used by representations *)

type lit =
  | LUnit
  | LBool  of bool
  | LInt   of int
  | LFloat of float
  | LSym   of string
  [@@deriving show]

and pattern =
  | PatLit of lit
  [@@deriving show]

and bin =
  | Add | Sub | Mul | Div | Mod
  | Eq  | Neq | Lt  | Lte | Gt | Gte
  | And | Or
  [@@deriving show]
