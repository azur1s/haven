open Common
open Utils
open Loc
open Lex

type cst =
  | CLit    of lit spanned
  | CList   of cst spanned list
  | CTuple  of cst spanned list
  | CBin    of cst spanned * bin * cst spanned
  | CApp    of cst spanned * cst spanned
  | CThen   of cst spanned * cst spanned
  | CLambda of
    { args: (string spanned * typ option) list
    ; ret: typ option
    ; body: cst spanned
    }
  | CIf of
    { cond: cst spanned
    ; t: cst spanned
    ; f: cst spanned
    }
  | CDef of
    { name: string spanned
    ; body: cst spanned
    ; typ: typ option
    ; in_: cst spanned
    }
  | CFun of
    { name: string spanned
    ; args: (string spanned * typ option) list
    ; body: cst spanned
    ; ret: typ option
    ; recr: bool
    ; in_: cst spanned
    }
  | CDestruct of
    { names: (string spanned * typ option) list
    ; body: cst spanned
    ; in_: cst spanned
    }
  | CCase of
    { value: cst spanned
    ; pats: (pattern spanned * cst spanned) list
    }

and cst_top =
  | CTUse of
    { path: string spanned list
    ; exposing: string spanned list option
    ; relative: bool
    }
  | CTDef of
    { name: string spanned
    ; body: cst spanned
    ; ret: typ option
    }
  | CTFun of
    { name: string spanned
    ; body: cst spanned
    ; args: (string spanned * typ option) list
    ; recr: bool
    ; ret: typ option
    }
  [@@deriving show]

type modul =
  { path: string list
  ; tops: cst_top spanned list
  }
  [@@deriving show]

type p =
  { input: token spanned list
  ; file: string
  ; mutable loc: int
  }

(* Location here mean the index to the input token array, not source characters *)
let make_span p start =
  { file = p.file
  ; start = start
  ; end_ = p.loc
  }

let peek p =
  if p.loc < List.length p.input then
    Some (p.input @* p.loc)
  else
    None

let advance p =
  if p.loc < List.length p.input then (
    p.loc <- p.loc + 1;
    Some (p.input @* (p.loc - 1))
  ) else
    None

let rewind p loc =
  p.loc <- loc

let advance_return p r =
  let _ = advance p in r

(* Used when the parser expect something and found end of file
   Go back to the previous token and grab it's end location for the error span*)
let eof_error_loc p =
  let prev_token = List.nth p.input (p.loc - 1) in
  make_span p (snd prev_token).end_

(* NOTE `expect_then` already advances for you, don't do `let _ = advance p`
   again inside the function to avoid skipping tokens *)
let expect_then p f expect_str =
  let rewind_pos = p.loc in
  match peek p with
  | Some t ->
    let _ = advance p in
    (match f t with
    | Ok v -> Ok v
    | Error e ->
      rewind p rewind_pos;
      Error e)
  | None -> err_ret ("Expected " ^ expect_str ^ ", found end of file") (eof_error_loc p)

let expect_cond p f expect_str =
  match peek p with
  | Some (t, s) when f t -> (Ok (t, s)) |> advance_return p
  | Some (t, s) -> err_ret ("Expected " ^ expect_str ^ ", found " ^ string_of_token t) s
  | None -> err_ret ("Expected " ^ expect_str ^ ", found end of file") (eof_error_loc p)

let expect p tk =
  expect_cond p ((=) tk) (string_of_token tk)

let maybe p tk =
  match expect_cond p ((=) tk) "" with
  | Ok t -> Some t
  | Error _ -> None

let parse_sym p =
  let* (sym, span) = expect_cond p (fun x -> match x with
    | TkSym _ -> true
    | _ -> false)
    "symbols"
  in match sym with
    | TkSym s -> Ok (s, span)
    | _ -> assert false

let many_delim p f delim =
  let rec many_acc p acc =
    match f p with
    | Ok v -> (
      match expect p delim with
      | Ok    _ -> many_acc p (v :: acc)
      | Error _ -> Ok (List.rev @@ v :: acc))
    | Error e -> Error e
  in
  many_acc p []

let many_until f p end_token =
  let rec aux acc =
    match peek p, f p with
    | Some (t, _), _ when t = end_token -> Ok (List.rev acc)
    | Some _, Ok v -> aux (v :: acc)
    | Some _, Error e -> Error e
    (* TODO error instead of returing parsed *)
    | None, _ -> Ok (List.rev acc)
  in
  aux []

let many_until_end f p =
  let rec aux acc =
    match peek p, f p with
    | Some _, Ok v -> aux (v :: acc)
    | Some _, Error e -> Error e
    | None, _ -> Ok (List.rev acc)
  in
  aux []

let many_cond p f =
  let rec many_cond_acc p acc =
    match peek p with
    | Some (t, s) when f t -> (
        let _ = advance p in
        many_cond_acc p ((t, s) :: acc))
    | _ -> Ok (List.rev acc)
  in
  many_cond_acc p []

let rec parse_typ p min_bp =
  let parse_typ_atom p =
    match peek p with
    | Some (TkSym s, span) ->
      (match s with
      | "unit"   -> Ok (TyConst "unit", span)
      | "bool"   -> Ok (TyConst "bool", span)
      | "int"    -> Ok (TyConst "int", span)
      | "float"  -> Ok (TyConst "float", span)
      | "string" -> Ok (TyConst "string", span)
      | s -> Ok (TyConst s, span))
      |> advance_return p
    | Some (TkOpen Paren, start) ->
      let _ = advance p in
      let* typ = parse_typ p 0 in
      let* (_, end_) = expect p @@ TkClose Paren in
      Ok (typ, span_union start end_)
    | Some (t, s) -> err_ret ("Expected type, found " ^ string_of_token t) s
    | None -> err_ret "Expected type, found end of file" (eof_error_loc p)
  in
  let rec parse_typ_loop lhs =
    let rewind_point = p.loc in
    match peek p with
    | Some (TkArrow, _) ->
      let power = 10 in
      if power < min_bp then
        Ok lhs
      else
        let _ = advance p in
        let* rhs = parse_typ p @@ power + 1 in
        (* Right-associative so keep parsing in the right direction *)
        let* rhs = parse_typ_loop rhs in
        Ok (TyArrow (lhs, rhs))
    | Some (TkBin Mul, _) ->
      let power = 20 in
      if power < min_bp then
        Ok lhs
      else
        let _ = advance p in
        let* rhs = parse_typ p @@ power + 1 in
        parse_typ_loop (TyTuple (lhs, rhs))
    | Some (TkSym _, _) ->
      (match parse_typ_atom p with
      | Ok (TyConst x, _) -> parse_typ_loop (TyConstructor (x, lhs))
      | Ok (_, where) ->
        rewind p rewind_point;
        err_ret "This type can't be used as a constructor" where
      | Error _ -> Ok lhs)
    | _ -> Ok lhs
  in
  let* (lhs, _) = parse_typ_atom p in
  parse_typ_loop lhs

let parse_let_args p =
  let rec parse_loop p acc =
    match peek p with
    | Some (TkSym s, span) ->
      let _ = advance p in
      parse_loop p @@ ((s, span), None) :: acc
    | Some (TkOpen Paren, _) ->
      let _ = advance p in
      let* sym = parse_sym p in
      let* _ = expect p TkColon in
      let* typ = parse_typ p 0 in
      let* _ = expect p @@ TkClose Paren in
      parse_loop p @@ (sym, Some typ) :: acc
    | _ ->
      if acc = [] then
        Ok None
      else
        Ok (Some (List.rev acc))
  in
  (* Handle special form `let f () = ...` *)
  match peek p with
  | Some (TkUnit, _) ->
    let _ = advance p in
    Ok (Some [])
  | _ -> parse_loop p []

let parse_case_pat p =
  let inf = "case pattern" in
  expect_then p (fun (t, span) -> match t with
  | TkUnit    -> Ok (PatLit LUnit, span)
  | TkBool  x -> Ok (PatLit (LBool x), span)
  | TkInt   x -> Ok (PatLit (LInt x), span)
  | TkFloat x -> Ok (PatLit (LFloat x), span)
  | TkStr   x -> Ok (PatLit (LStr x), span)
  | TkSym   x -> Ok (PatLit (LSym x), span)
  | t -> err_ret ("Expected " ^ inf ^ ", found " ^ string_of_token t) span)
  inf

let rec parse_atom p =
  match peek p with
  | Some (t, span) -> (match t with
    | TkUnit    -> advance_return p (Ok (CLit (LUnit, span), span))
    | TkBool  x -> advance_return p (Ok (CLit (LBool x, span), span))
    | TkInt   x -> advance_return p (Ok (CLit (LInt x, span), span))
    | TkFloat x -> advance_return p (Ok (CLit (LFloat x, span), span))
    | TkStr   x -> advance_return p (Ok (CLit (LStr x, span), span))
    | TkSym   x -> advance_return p (Ok (CLit (LSym x, span), span))
    (* (expr, ...) *)
    | TkOpen Paren ->
      let _ = advance p in
      let* exp = parse_expr p 0 in
      (match peek p with
      | Some (TkClose Paren, end_span) ->
        advance_return p @@ Ok (fst exp, span_union span end_span)
      | Some (TkComma, _) ->
        let _ = advance p in
        let* exprs = many_delim p (fun p -> parse_expr p 0) TkComma in
        let* (_, end_span) = expect p (TkClose Paren) in
        Ok (CTuple (exp :: exprs), (span_union span end_span))
      | Some (t, s) -> err_ret ("Expected `)` or `,`, found " ^ string_of_token t) s
      | None -> err_ret ("Expected `)` or `,`, found end of file") (eof_error_loc p))
    | TkOpen Brack ->
      let _ = advance p in
      (match peek p with
      | Some (TkClose Brack, end_span) ->
        let _ = advance p in
        Ok (CList [], span_union span end_span)
      | _ ->
        let* exprs = many_delim p (fun p -> parse_expr p 0) TkComma in
        let* (_, end_span) = expect p (TkClose Brack) in
        Ok (CList exprs, span_union span end_span))
    | TkFun ->
      let _ = advance p in
      let* args = parse_let_args p in
      let args = (match args with
        | Some args -> args
        | None -> []) in
      let colon = maybe p TkColon in
      let* ret = if Option.is_some colon
        then Result.map Option.some (parse_typ p 0)
        else Ok None
      in
      let* _ = expect p TkArrow in
      let* body = parse_expr p 0 in
      Ok (CLambda { args; ret; body }, span_union span (snd body))
    | TkIf ->
      let _ = advance p in
      let* cond = parse_expr p 0 in
      let* _ = expect p TkThen in
      let* t = parse_expr p 0 in
      let* _ = expect p TkElse in
      let* f = parse_expr p 0 in
      Ok (CIf { cond; t; f; }, span_union span (snd f))
    | TkLet ->
      let _ = advance p in
      (match peek p with
      | Some (TkOpen Paren, _) ->
        let _ = advance p in
        let* names_tys = parse_let_args p in
        let* _ = if Option.is_none names_tys
          then err_ret "Expected variable names" span
          else Ok ()
        in
        let* _ = expect p (TkClose Paren) in
        let* _ = expect p TkAssign in
        let* body = parse_expr p 0 in
        let* _ = expect p TkIn in
        let* in_ = parse_expr p 0 in
        Ok (CDestruct
          { names = Option.get names_tys
          ; body
          ; in_
          }, span_union span (snd in_))
      | _ ->
        let rec_tk = maybe p TkRec in
        let recr = if Option.is_some rec_tk
          then true
          else false
        in
        let* name = parse_sym p in
        let* args = parse_let_args p in
        let* _ = (match rec_tk, args with
          | Some (_, s), None -> err_ret "Variable definition can't be recursive" s
          | _, _ -> Ok ()) in
        let colon = maybe p TkColon in
        let* typ = if Option.is_some colon
          then Result.map Option.some (parse_typ p 0)
          else Ok None
        in
        let* _ = expect p TkAssign in
        let* body = parse_expr p 0 in
        let* _ = expect p TkIn in
        let* in_ = parse_expr p 0 in
        (match args with
        | None -> Ok (CDef
          { name
          ; body
          ; typ
          ; in_
          }, span_union span (snd in_))
        | Some args -> Ok (CFun
          { name
          ; body
          ; args
          ; ret = typ
          ; in_
          ; recr
          }, span_union span (snd in_))))
    | TkCase ->
      let parse_case_clause p tk =
        let inf = "case clause" in
        expect_then p (fun (t, span) -> match t with
        | t when t = tk ->
          let* pat = parse_case_pat p in
          let* _ = expect p TkArrow in
          let* exp = parse_expr p 0 in
          Ok (pat, exp)
        | t -> err_ret ("Expected " ^ inf ^ ", found " ^ string_of_token t) span)
        inf
      in

      let _ = advance p in
      let* value = parse_expr p 0 in
      let* _ = expect p TkOf in
      let* pats = many_until (fun p -> parse_case_clause p TkBar) p TkBarElse in
      let* else_ = parse_case_clause p TkBarElse in
      Ok (CCase
        { value
        ; pats = pats @ [else_]
        }, span_union span (snd @@ snd else_))
    | t -> err_ret ("Expected expression, found " ^ string_of_token t) span)
  | None -> err_ret "Expected expression, found end of file" (eof_error_loc p)

(* https://ocaml.org/manual/5.2/expr.html#ss:precedence-and-associativity *)
and binding_power = function
  | Mul | Div | Mod     -> 120, 121
  | Add | Sub           -> 110, 111
  | Cons                -> 100, 101
  | Eq | Neq
  | Lt | Lte | Gt | Gte -> 80, 81
  | And | Or            -> 70, 71

and parse_expr p min_bp =
  let rec parse_loop lhs =
    let rewind_point = p.loc in
    match peek p with
    (* Binary operator *)
    | Some (TkBin bin, _) ->
      let l_pw, r_pw = binding_power bin in
      if l_pw < min_bp then
        Ok lhs
      else
        let _ = advance p in
        let* rhs = parse_expr p r_pw in
        parse_loop (CBin (lhs, bin, rhs), span_union (snd lhs) (snd rhs))
    (* Semicolon *)
    | Some (TkSemi, _) ->
      let l_pw, r_pw = 1, 2 in
      if l_pw < min_bp then
        Ok lhs
      else
        let _ = advance p in
        let* rhs = parse_expr p r_pw
          |> Result.map_error (with_hint "If you meant to return unit, then add a () expression after the semicolon")
        in
        parse_loop (CThen (lhs, rhs), span_union (snd lhs) (snd rhs))
    (* Application *)
    | Some _ ->
      (* Try parse, if goes wrong then just return lhs *)
      let l_pw, r_pw = 150, 151 in
      if l_pw < min_bp then
        Ok lhs
      else
        (match parse_expr p r_pw with
        | Ok x ->
          parse_loop (CApp (lhs, x), span_union (snd lhs) (snd x))
        | Error _ ->
          rewind p rewind_point;
          Ok lhs)
    | None -> Ok lhs
  in

  let* lhs = parse_atom p in
  parse_loop lhs

and parse_top p =
  match peek p with
  | Some (t, span) -> (match t with
    | TkUse ->
      let _ = advance p in
      let* relative = match peek p with
        | Some (TkDot, _) ->
          let _ = advance p in
          let* _ = expect p (TkBin Div) in
          Ok true
        | _ -> Ok false in
      let rec parse_path p acc =
        let* sym = parse_sym p in
        let acc = acc @ [sym] in
        match peek p with
        | Some (TkBin Div, _) ->
          let _ = advance p in
          parse_path p acc
        | _ -> Ok acc
      in
      let* path = parse_path p [] in
      let exposing = maybe p (TkOpen Paren) in
      let* (exposing, end_span) = match exposing with
        | Some _ ->
          let* syms = many_until (fun p -> parse_sym p) p (TkClose Paren) in
          let* (_, end_span) = expect p (TkClose Paren) in
          Ok (Some syms, end_span)
        | None ->
          let span = snd @@ List.nth path (List.length path - 1) in
          Ok (None, span)
      in
      Ok (CTUse { path; exposing; relative }, span_union span end_span)
    | TkLet ->
      let _ = advance p in
      let rec_tk = maybe p TkRec in
      let recr = if Option.is_some rec_tk
        then true
        else false
      in
      let* name = parse_sym p in
      let* args = match peek p with
        | Some (TkUnit, s) -> Ok (Some [(("_unit_", s), Some (TyConst "unit"))])
          |> advance_return p
        | _ -> parse_let_args p
      in
      let* _ = (match rec_tk, args with
        | Some (_, s), None -> err_ret "Variable definition can't be recursive" s
        | _, _ -> Ok ()) in
      let colon = maybe p TkColon in
      let* ret = if Option.is_some colon
        then Result.map Option.some (parse_typ p 0)
        else Ok None
      in
      let* _ = expect p TkAssign in
      let* body = parse_expr p 0 in
      (match args with
      | None -> Ok (CTDef
        { name
        ; body
        ; ret
        }, span_union span (snd body))
      | Some args -> Ok (CTFun
        { name
        ; body
        ; args
        ; ret
        ; recr
        }, span_union span (snd body)))
    | t -> err_ret ("Expected top level statement, found " ^ string_of_token t) span)
  | None -> err_ret "Expected top level statement, found end of file" (eof_error_loc p)

and parse_tops p =
  many_until_end parse_top p

let parse ?(file="<anonymous>") tks =
  let p = { input = tks; file = file; loc = 0 } in
  let res = parse_tops p in
  match res with
  | Ok v ->
    let uses = List.filter_map (function
      | (CTUse { path; relative; _ }, _) ->
        let path = String.concat "/" @@ List.map fst path in
        if relative
          then Some ("./" ^ path)
          else Some path
      | _ -> None) v in
    if List.length p.input = p.loc - 1
      then Ok (v, uses)
      else let next = advance p in
        (match next with
        | Some (t, s) -> err_ret ("Unexpected " ^ string_of_token t ^ ", expected end of file") s
        | None -> Ok (v, uses))
  | Error e -> Error e