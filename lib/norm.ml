open Common
open Utils
open Infer

type kterm =
  | KLit   of lit
  | KList  of kterm list
  | KTuple of kterm list
  | KBin   of kterm * bin * kterm
  | KApp   of kterm * kterm list
  | KThen  of kterm * kterm
  | KIf of
    { cond: kterm
    ; t: kterm
    ; f: kterm
    }
  | KDef of
    { name: string
    ; body: kterm
    ; in_: kterm
    }
  | KFun of
    { name: string
    ; args: string list
    ; body: kterm
    ; in_: kterm
    ; recr: bool
    }
  | KDestruct of
    { names: string list
    ; body: kterm
    ; in_: kterm
    }
  | KCase of
    { value: kterm
    ; pats: (pattern * kterm) list
    ; else_: kterm
    }
  [@@deriving show]

and ktop =
  | KTDef of string * kterm
  | KTFun of
    { name: string
    ; args: string list
    ; recr: bool
    ; body: kterm
    }
  [@@deriving show]

let rec uncurry t args =
  match t with
  | KApp (f, x) -> uncurry f (x @ args)
  | _ -> (t, args)

let rec norm_term term =
  match (fst term) with
  | TLit (l, _) -> KLit l
  | TList l -> KList (List.map norm_term l)
  | TTuple l -> KTuple (List.map norm_term l)
  | TBin (a, op, b) -> KBin (norm_term a, op, norm_term b)
  | TThen (a, b) -> KThen (norm_term a, norm_term b)
  (* Uncurry applications *)
  | TApp (f, x) -> uncurry (norm_term f) [norm_term x]
    |> fun (f, x) -> KApp (f, x)
  | TIf { cond; t; f; _ } -> KIf
    { cond = norm_term cond
    ; t = norm_term t
    ; f = norm_term f }
  | TDef { name; body; in_; _ } -> KDef
    { name = fst name
    ; body = norm_term body
    ; in_ = norm_term in_ }
  | TFun { name; args; recr; body; in_; _ } -> KFun
    { name = fst name
    ; args = List.map (fun x -> fst @@ fst x) args
    ; recr
    ; body = norm_term body
    ; in_ = norm_term in_ }
  | TDestruct { names; body; in_; _ } -> KDestruct
    { names = List.map fst names
    ; body = norm_term body
    ; in_ = norm_term in_ }
  | e -> todo __LOC__ ~reason:(show_term e)

let norm_top top =
  match top with
  | TTDef { name; body; _ } ->
    norm_term body
    |> fun body ->
      KTDef (fst name, body)
  | TTFun { name; args; recr; body; _ } -> KTFun
    { name = fst name
    ; body = norm_term body
    ; recr
    ; args = List.map (fun x -> fst @@ fst x) args
    }

let norm tops =
  List.map (fun x -> norm_top @@ fst x) tops
