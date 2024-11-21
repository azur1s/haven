open Common
open Utils
open Loc
open Infer
open Set

type kterm =
  | KLit   of lit
  | KList  of kterm list
  | KBin   of kterm * bin * kterm
  | KApp   of kterm * kterm
  | KIf of
    { cond: kterm
    ; t: kterm
    ; f: kterm
    }
  | KBlock of kterm list
  | KLet of
    { name: string
    ; body: kterm
    ; args: string list option
    ; in_: kterm
    }
  | KCase of
    { value: kterm
    ; pats: (pattern * kterm) list
    ; else_: kterm
    }

(* TODO K-normalization (turns nested expression into multiple lets) *)
