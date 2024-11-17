open Common
open Loc

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
  | TBlock of term spanned list
  | TLet of
    { name: string spanned
    ; body: term spanned * typ
    ; args: (string spanned * typ) list
    ; ret: typ
    ; in_: term spanned
    }
  | TCase of
    { value: term spanned
    ; pats: (pattern spanned * term spanned) list
    ; else_: term spanned
    }
  [@@deriving show]

and typ =
  | TyUnit
  | TyBool
  | TyInt
  | TyFloat
  | TyCustom of string
  | TyArrow of typ * typ