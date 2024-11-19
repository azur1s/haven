open Common
open Utils
open Loc
open Parse

type term =
  | TLit   of lit
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
    ; body: term spanned
    ; args: (string spanned * typ) list option
    ; ret: typ
    ; in_: term spanned
    }
  | TCase of
    { value: term spanned
    ; pats: (pattern spanned * term spanned) list
    ; else_: term spanned
    }
  [@@deriving show]

and term_top =
  | TTLet of
    { name: string spanned
    ; body: term spanned
    ; args: (string spanned * typ) list option
    ; ret: typ
    }
  [@@deriving show]

(* TODO type infer *)
let rec infer_unwrap cst =
  let span = snd cst in
  let oks a = Ok (a, span) in

  match (fst cst) with
  | CLit l -> oks @@ TLit l
  | CBin (a, op, b) ->
    let* a = infer_unwrap a in
    let* b = infer_unwrap b in
    oks @@ TBin (a, op, b)
  | CApp (f, x) ->
    let* f = infer_unwrap f in
    let* x = infer_unwrap x in
    oks @@ TApp (f, x)
  | CIf { cond; t; f } ->
    let* cond = infer_unwrap cond in
    let* t = infer_unwrap t in
    let* f = infer_unwrap f in
    oks @@ TIf { cond; t; f }
  | CBlock cs ->
    let* ts = map_early_return infer_unwrap cs in
    oks @@ TBlock ts
  | CLet { name; body; args = None; ret; in_ } ->
    let* body = infer_unwrap body in
    let* in_ = infer_unwrap in_ in
    let* ret = match ret with
    | Some t -> Ok t
    | None -> Error ("Need return type here", (snd name))
    in
    oks @@ TLet { name; body; args = None; ret; in_ }
  | CLet { name; body; args = Some (args); ret; in_ } ->
    let* body = infer_unwrap body in
    let* in_ = infer_unwrap in_ in
    let* args = map_early_return (fun (arg, t) ->
      match t with
      | Some t -> Ok (arg, t)
      | None -> Error ("Need type here", (snd arg))
    ) args in
    let* ret = match ret with
    | Some t -> Ok t
    | None -> Error ("Need return type here", (snd name))
    in
    oks @@ TLet { name; body; args = Some args ; ret; in_ }
  | CCase { value; pats; else_ } ->
    let* value = infer_unwrap value in
    let* pats = map_early_return
      (fun (pat, e) -> let* e = infer_unwrap e in Ok (pat, e)) pats
    in
    let* else_ = infer_unwrap else_ in
    oks @@ TCase { value; pats; else_ }

let infer_unwrap_top cst_top =
  let span = snd cst_top in
  let oks a = Ok (a, span) in

  match (fst cst_top) with
  | CTLet { name; body; args = None; ret } ->
    let* body = infer_unwrap body in
    let* ret = match ret with
    | Some t -> Ok t
    | None -> Error ("Need return type here", (snd name))
    in
    oks @@ TTLet { name; body; args = None; ret }
  | CTLet { name; body; args = Some (args); ret } ->
    let* body = infer_unwrap body in
    let* args = map_early_return (fun (arg, t) ->
      match t with
      | Some t -> Ok (arg, t)
      | None -> Error ("Need type here", (snd arg))
    ) args in
    let* ret = match ret with
    | Some t -> Ok t
    | None -> Error ("Need return type here", (snd name))
    in
    oks @@ TTLet { name; body; args = Some args ; ret }

let infer tops =
  map_sep_results @@ List.map infer_unwrap_top tops
