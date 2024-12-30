open Common
open Utils
open Infer

type kterm =
  | KLit   of lit
  | KList  of kterm list
  | KTuple of kterm list
  | KBin   of kterm * bin * kterm
  | KApp   of kterm * kterm list
  | KLambda of string list * kterm
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
    ; recr: bool
    ; body: kterm
    ; in_: kterm
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
  [@@deriving show, sexp_of]

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

let flat_map f l = List.map f l |> List.flatten

let rec norm_term term =
  match (fst term) with
  | TLit (l, _) -> KLit l
  | TList l -> KList (List.map norm_term l)
  | TTuple l -> KTuple (List.map norm_term l)
  | TBin (a, op, b) ->
    let a = norm_term a in
    let b = norm_term b in
    KBin (a, op, b)
  | TThen (a, b) ->
    let a = norm_term a in
    let b = norm_term b in
    KDef { name = "_then_"; body = a; in_ =  b }
  (* Uncurry applications *)
  | TApp (f, x) -> uncurry (norm_term f) [norm_term x]
    |> fun (f, x) -> KApp (f, x)
  | TLambda { args; body; _ } -> KLambda
    (List.map (fun x -> fst @@ fst x) args, norm_term body)
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

let rec flatten_let = function
  (*
  Example:
  let a =
    let b =
      let c = 1
      in c
    in b
  in a
  =>
  let c = 1
  in let b = c
  in let a = b
  in a
  *)
  | KDef { name; body = KDef { name = name2; body = body2; in_ = in2 }; in_ } ->
    let ubody2 = flatten_let body2 |> flatten_let in
    let uin2 = flatten_let in2 in
    flatten_let @@ KDef
      { name = name2
      ; body = ubody2
      ; in_ = KDef
        { name = name
        ; body = uin2
        ; in_ = flatten_let in_
        } }
  | KDef { name; body; in_ } ->
    KDef { name; body = flatten_let body; in_ = flatten_let in_ }
  | x -> x

let rec fun_to_lambda_def = function
  | KFun { name; args; body; in_; recr = _recr } ->
    let body = fun_to_lambda_def body in
    let in_ = fun_to_lambda_def in_ in
    KDef
      { name
      ; body = KLambda (args, body)
      ; in_ }
  | x -> x

let norm_top top =
  let norm t =
    norm_term t
    |> flatten_let
    |> fun_to_lambda_def
  in
  match top with
  | TTDef { name; body; _ } ->
    norm body
    |> fun body -> KTDef (fst name, body)
  | TTFun { name; args; recr; body; _ } -> KTFun
    { name = fst name
    ; body = norm body
    ; recr
    ; args = List.map (fun x -> fst @@ fst x) args
    }

let norm tops =
  List.map (fun x -> norm_top @@ fst x) tops
