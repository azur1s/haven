open Common
open Utils
open Loc
open Infer
open Set

type kterm =
  | KLit   of lit
  | KList  of kterm list
  | KBin   of kterm * bin * kterm
  | KApp   of kterm * kterm list
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
  [@@deriving show]

and ktop =
  | KTDef of string * kterm
  | KTLet of
    { name: string
    ; args: string list
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
  | TBin (a, op, b) -> KBin (norm_term a, op, norm_term b)
  (* Uncurry applications *)
  | TApp (f, x) ->
    let (f, x) = uncurry (norm_term f) [norm_term x] in
    KApp (f, x)
  | TLet { name; args; body; in_; _ } -> KLet
    { name = fst name
    ; body = norm_term body
    ; args = (match args with
      | None -> None
      | Some args -> Some (List.map (fun x -> fst @@ fst x) args))
    ; in_ = norm_term in_ }
  | e -> todo __LOC__ ~reason:(show_term e)

let norm_top top =
  match top with
  | TTLet { name; args = None; body; _ } ->
    KTDef (fst name, norm_term body)
  | TTLet { name; args = Some args; body; _ } ->
    KTLet { name = fst name
         ; body = norm_term body
         ; args = List.map (fun x -> fst @@ fst x) args
         }

let norm tops =
  List.map (fun x -> norm_top @@ fst x) tops
