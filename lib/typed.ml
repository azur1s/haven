open Loc
open Lex

type term =
  | TUnit
  | TBool  of bool
  | TInt   of int
  | TFloat of float
  | TSym   of string
  | TBin   of term spanned * bin * term spanned
  | TApp   of term spanned * term spanned
  | TIf of
    { cond: term spanned
    ; t: term spanned
    ; f: term spanned
    }
  | CBlock of term spanned list
  | CLet of
    { name: string spanned
    ; body: term spanned * typ
    ; args: (string * typ) spanned list
    ; in_: term spanned
    }
  [@@deriving show]

and typ =
  | TyUnit
  | TyBool
  | TyInt
  | TyFloat
  | TyCustom of string
  | TyArrow of typ * typ