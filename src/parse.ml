open Common
open Lex
open Types

type cst =
  | CValue  of value
  | CSym    of string
  | CApp    of cst spanned * cst spanned
  | CBin    of cst spanned * bin * cst spanned
  | CLambda of string spanned * cst spanned
  | CCase of
    { value: cst spanned
    ; br: (value spanned * cst spanned) list
    ; default_br: cst spanned
    }
  | CLet of
    { name: string spanned
    ; value: cst spanned
    ; body: cst spanned
    }
  | CThen of cst spanned * cst spanned
  [@@deriving show]

type ctop =
  | CTUse  of string spanned
  | CTAnno of string spanned * tp spanned
  | CTDef  of string spanned * cst spanned
  [@@deriving show]

(* parser & functions *)

type parser =
  { file: string
  ; tokens: (token * span) list
  ; mutable loc: int
  }
  [@@deriving show]

let parser_init file tokens =
  { file; tokens; loc = 0 }

let peek p =
  if p.loc < List.length p.tokens then
    Some (List.nth p.tokens p.loc)
  else
    None

let advance p =
  if p.loc < List.length p.tokens then (
    p.loc <- p.loc + 1;
    Some (List.nth p.tokens (p.loc - 1))
  ) else None

let rewind p loc =
  p.loc <- loc

let advance_return p r =
  let _ = advance p in r

(* Used when the parser expect something and found end of file, go back to the
   previous token and grab it's end location for the error span *)
let eof_error_loc p =
  let loc =
    match p.tokens with
    (* If there are no tokens, we return a default location. *)
    | [] -> { file = p.file; pos_start = (0, 0); pos_end = (0, 0) }
    (* If there are tokens, we return the end location of the last token. *)
    | _ -> snd (List.nth p.tokens (p.loc - 1))
  in { file = p.file; pos_start = loc.pos_start; pos_end = loc.pos_end }

(** [many0 f p] Parse zero or more times with the parser [f]. If the parser
    fails or there are no more tokens then it returns the accumulated values.
  - [f] the parser to be applied zero or more times
  - [p] the input stream
*)
let many0 f p =
  let rec aux acc =
    match peek p, f p with
    | Some _, Ok v -> aux (v :: acc)
    | Some _, Error _ | None, _ -> Ok (List.rev acc)
  in aux []

(** Like `many0` but only ends when the parser [f] ran out of input. If the
    parser fails then the error is returned.
  - [f] the parser to be applied zero or more times
  - [p] the input stream
*)
let many0_no_err f p =
  let rec aux acc =
    match peek p, f p with
    | Some _, Ok v -> aux (v :: acc)
    | Some _, Error e -> Error e
    | None, _ -> Ok (List.rev acc)
  in aux []

let many1 f p =
  let* xs = many0 f p in
  match xs with
  | [] -> err_ret "Expected at least one value" (eof_error_loc p)
  | _ -> Ok xs

(** [many_until f p end_token] Parse multiple values with the parser [f] until
    it encounters the specified [end_token].
  - [f] is a function that takes a parser [p] and returns a result of type ['a].
  - [p] is a parser that provides a stream of tokens.
  - [end_token] is the token that marks the end of the parsing process.
    @example {[many_until (fun p -> parse_int p) p Semicolon]}
*)
let many_until f p end_token =
  let rec aux acc =
    match peek p, f p with
    (* If peek returns the end_token, we stop parsing and return the accumulated
       values. *)
    | Some (t, _), _ when t = end_token -> Ok (List.rev acc)
    (* If peek returns a token and f returns Ok, we add the value to the
       accumulator and continue parsing. *)
    | Some _, Ok v -> aux (v :: acc)
    (* If peek returns a token and f returns an Error, we stop parsing and
       return the error. *)
    | Some _, Error e -> Error e
    (* If there's no more tokens, then we error out saying we expected
       the end_token. *)
    | None, _ ->
      err_ret
        ("Expected " ^ string_of_token end_token ^ " but got end of file")
        (eof_error_loc p)
  in aux []

(** [many_until_end f p] Parse multiple values with the parser [f] until it
    encounters the end of the input stream.
  - [f] is a function that takes a parser [p] and returns a result of type ['a].
  - [p] is a parser that provides a stream of tokens.
    @example {[many_until_end (fun p -> parse_int p) p]}
*)
let many_until_end f p =
  let rec aux acc =
    match peek p, f p with
    | Some _, Ok v -> aux (v :: acc)
    | Some _, Error e -> Error e
    | None, _ -> Ok (List.rev acc)
  in aux []

(** [expect_cond p f expect_str] Parse the next token and check if it satisfies
    the condition [f]. Does not consume the token if it does not satisfy the
    condition.
  - [p] is a parser that provides a stream of tokens.
  - [f] is a function that takes a token and returns a boolean.
  - [expect_str] is a string that describes what was expected for reporting.
*)
let satisfy p f expect_str =
  match peek p with
  | Some (t, s) when f t -> (Ok (t, s)) |> advance_return p
  | Some (t, s) -> err_ret
    ("Expected " ^ expect_str ^ ", found " ^ string_of_token t) s
  | None -> err_ret
    ("Expected " ^ expect_str ^ ", found end of file") (eof_error_loc p)

let just p tk =
  match peek p with
  | Some (t, s) when t = tk ->
    advance_return p (Ok (tk, s))
  | Some (t, s) -> err_ret
    ("Expected " ^ string_of_token tk ^ " but got " ^ string_of_token t) s
  | None -> err_ret
    ("Expected " ^ string_of_token tk ^ " but got end of file")
    (eof_error_loc p)

let many_delim p f delim =
  let rec many_acc p acc =
    match f p with
    | Ok v -> (
      match just p delim with
      | Ok    _ -> many_acc p (v :: acc)
      | Error _ -> Ok (List.rev @@ v :: acc))
    | Error _ -> Ok (List.rev acc)
  in
  many_acc p []

let or_else p f1 f2 =
  let before = p.loc in
  match f1 p with
  | Ok v -> Ok v
  | Error _ ->
    p.loc <- before;
    f2 p

(* parsing *)

let parse_sym p =
  let* (sym, span) = satisfy p (fun x -> match x with
    | TkSym _ -> true
    | _ -> false)
    "a symbol"
  in match sym with
    | TkSym s -> Ok (s, span)
    | _ -> assert false

let parse_value p =
  match peek p with
  | Some (t, span) -> (match t with
    | TkUnit    -> advance_return p (Ok (VUnit, span))
    | TkBool  x -> advance_return p (Ok (VBool x, span))
    | TkInt   x -> advance_return p (Ok (VInt x, span))
    | TkStr   x -> advance_return p (Ok (VStr x, span))
    | t -> err_ret ("Expected literal, found " ^ string_of_token t) span)
  | None -> err_ret "Expected literal, found end of file" (eof_error_loc p)

let rec parse_tp p min_bp =
  let parse_tp_atom p =
    match peek p with
    | Some (t, span) -> (match t with
      | TkSym s -> advance_return p (Ok (constr s, span))
      | _ -> err_ret ("Expected a type, found " ^ string_of_token t) span)
    | None -> err_ret "Expected a type, found end of file" (eof_error_loc p)
  in
  let rec parse_tp_loop lhs =
    match peek p with
    | Some (TkArrow, _) ->
      let power = 10 in
      if power < min_bp then
        Ok lhs
      else
        let _ = advance p in
        let* rhs = parse_tp p (power + 1) in
        (* Right-associative so keep parsing in the right direction *)
        let* (rhs, s) = parse_tp_loop rhs in
        Ok (arrow (fst lhs) [rhs], span_union (snd lhs) s)
    | Some (TkSym f, sp) -> failwith ("TODO: type application: " ^ f ^ " at " ^ string_of_span sp)
    | _ -> Ok lhs
  in
  let* lhs = parse_tp_atom p in
  parse_tp_loop lhs

let rec parse_atom p =
  let value p = parse_value p |> Result.map (fun (v, s) -> (CValue v, s)) in

  let atom p = (match peek p with
  | Some (t, s) -> (match t with
    | TkSym x -> advance_return p (Ok (CSym x, s))

    | TkLambda ->
      advance p |> ignore;
      (* let* arg = parse_sym p in *)
      let* args = many1 parse_sym p in
      let* _ = just p TkArrow in
      let* body = parse_expr p 0 in
      (* Ok (CLambda (arg, body), span_union s (snd body)) *)
      let lambda =
        List.fold_right (fun arg acc ->
          (CLambda (arg, acc), span_union s (snd acc))
        ) args body
      in
      Ok lambda

    | TkCase ->
      let parse_br p =
        let* _ = just p TkBar in
        let* value = parse_value p in
        let* _ = just p TkArrow in
        let* body = parse_expr p 0 in
        Ok (value, body)
      in
      let parse_br_default p =
        let* _ = just p TkElse in
        let* body = parse_expr p 0 in
        Ok body
      in

      advance p |> ignore;
      let* value = parse_expr p 0 in
      let* branches = many1 parse_br p in
      let* default_br = parse_br_default p in
      let span =
        match branches with
        | [] -> s
        | (_, (_, end_s)) :: _ -> span_union s end_s
      in
      Ok (CCase { value; br = branches; default_br }, span)

    | TkLet ->
      advance p |> ignore;
      let* name = parse_sym p in
      let* args = many0 parse_sym p in
      let* _ = just p TkAssign in
      let* value = parse_expr p 0 in
      let* _ = just p TkIn in
      let* body = parse_expr p 0 in
      if args = [] then
        Ok (CLet { name; value; body }, span_union s (snd body))
      else
        let lambda_value = List.fold_right (fun arg acc ->
          (CLambda (arg, acc), span_union s (snd acc))
        ) args value
        in
        Ok (CLet { name; value = lambda_value; body }, span_union s (snd body))

    (* (expr) *)
    | TkL '(' ->
      advance p |> ignore;
      let* e = parse_expr p 0 in
      (match peek p with
      | Some (TkR ')', end_s) ->
        advance_return p (Ok (fst e, span_union s end_s))
      | Some (t, s) -> err_ret ("Expected ')', found " ^ string_of_token t) s
      | None -> err_ret "Expected ')', found end of file" (eof_error_loc p))
    | t -> err_ret ("Expected an expression but got " ^ string_of_token t) s)
  | None -> err_ret "Expected a token but got end of file" (eof_error_loc p))
  in

  or_else p value atom

(* https://ocaml.org/manual/5.2/expr.html#ss:precedence-and-associativity *)
and binding_power = function
  (*Access f.a          -> 200, 201 *)
  | Mul | Div | Mod     -> 130, 131
  | Add | Sub           -> 120, 121
  (*PipeL               -> 110, 111 *)
  (* | Cons                -> 100, 101 *)
  | Eq | Neq            -> 80, 81
  | Lt | Le | Gt | Ge   -> 80, 81
  | And | Or            -> 70, 71
  (*Semicolon           -> 1, 2 *)

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
        | Ok rhs ->
          parse_loop (CApp (lhs, rhs), span_union (snd lhs) (snd rhs))
        | Error _ ->
          rewind p rewind_point;
          Ok lhs)
    | None -> Ok lhs
  in

  let* lhs = parse_atom p in
  parse_loop lhs

let parse_top p =
  match peek p with
  | Some (t, s) -> (match t with
    | TkUse ->
      let* _ = just p TkUse in
      let* name = satisfy p (fun x -> match x with
        | TkStr _ -> true
        | _ -> false)
        "a symbol"
      in
      (match name with
      | TkStr name, span ->
        let* _ = just p TkDot in
        Ok (CTUse (name, span), span_union s span)
      | _ -> assert false)
    | TkSym _ ->
      let* name = parse_sym p in
      let* args = many0 parse_sym p in
      if args = [] then
        (* = or : *)
        let* typ = or_else p
          (fun p -> just p TkColon)
          (fun p -> just p TkAssign)
          |> Result.map (fun (tk, _) ->
            match tk with
            | TkAssign -> true
            | TkColon  -> false
            | _ -> assert false)
        in
        (match typ with
        | true ->
          let* body = parse_expr p 0 in
          let* _ = just p TkDot in
          Ok (CTDef (name, body), span_union s (snd body))
        | false ->
          let* tp = parse_tp p 0 in
          let* _ = just p TkDot in
          Ok (CTAnno (name, tp), span_union s (snd tp))
        )
      else
        let* _ = just p TkAssign in
        let* body = parse_expr p 0 in
        let* _ = just p TkDot in
        let lambda_body = List.fold_right (fun arg acc ->
          (CLambda (arg, acc), span_union s (snd acc))
        ) args body
        in
        Ok (CTDef (name, lambda_body), span_union s (snd body))
    | t -> err_ret
      ("Expected a top-level statement, found " ^ string_of_token t) s)
  | None -> Error (err "Expected a top-level statement, found end of file" (eof_error_loc p))

let parse ?(file="<anonymous>") tks =
  let p = parser_init file tks in
  let res = many0_no_err parse_top p in
  res