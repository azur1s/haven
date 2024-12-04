open Common
open Utils
open Norm

type erl_expr =
  | ELit of lit
  | EList of erl_expr list
  | ETuple of erl_expr list
  | EBin of erl_expr * bin * erl_expr
  | EApp of erl_expr * erl_expr list * bool
  (* fun name(args) -> e end *)
  | ELam of string * string list * erl_expr
  (* e1, e2. *)
  | EThen of erl_expr * erl_expr
  (* x = e *)
  | ELet of string * erl_expr
  | ELetD of string list * erl_expr
  | EIf of erl_expr * erl_expr * erl_expr
  | EInline of string
  [@@deriving show]

and erl_top =
  (* -export([f/i]) *)
  | EExport of (string * int) list
  | EDef of string * erl_expr
  (* fun(args) -> e. *)
  | EFun of
    { name: string
    ; args: string list
    ; expr: erl_expr
    }
  [@@deriving show]

let rec string_of_erl_expr ?(cap=true) = function
  | ELit (LSym s) when cap -> capitalize_first s
  | ELit l -> string_of_lit l
  | EList l -> Printf.sprintf "[%s]" (String.concat ", " (List.map string_of_erl_expr l))
  | ETuple l -> Printf.sprintf "{%s}" (String.concat ", " (List.map string_of_erl_expr l))
  | EBin (a, Cons, b) ->
    Printf.sprintf "[%s | %s]"
      (string_of_erl_expr a)
      (string_of_erl_expr b)
  | EBin (a, op, b) ->
    Printf.sprintf "%s %s %s"
      (string_of_erl_expr a)
      (string_of_bin op)
      (string_of_erl_expr b)
  | EApp (f, args, is_top) ->
    Printf.sprintf "%s(%s)"
      (string_of_erl_expr f ~cap:(not is_top))
      (String.concat ", " (List.map string_of_erl_expr args))
  | ELam (name, args, e) ->
    Printf.sprintf "fun %s(%s) -> %s end"
      (capitalize_first name)
      (String.concat ", " (List.map capitalize_first args))
      (string_of_erl_expr e)
  | EThen (e1, e2) ->
    Printf.sprintf "%s,\n  %s" (string_of_erl_expr e1) (string_of_erl_expr e2)
  | ELet (x, e) -> Printf.sprintf "%s = %s" (capitalize_first x) (string_of_erl_expr e)
  | ELetD (xs, e) -> Printf.sprintf "{%s} = %s" (String.concat ", " (List.map capitalize_first xs)) (string_of_erl_expr e)
  | EIf (e, t, f) ->
    Printf.sprintf "if\n    %s -> %s;\n    true -> %s\n  end"
      (string_of_erl_expr e)
      (string_of_erl_expr t)
      (string_of_erl_expr f)
  | EInline s -> s

let string_of_erl_top = function
  | EExport exports ->
    Printf.sprintf "-export([%s]).\n"
      (String.concat ", " (List.map (fun (f, a) -> Printf.sprintf "%s/%d" f a) exports))
  | EDef (x, e) -> Printf.sprintf "%s() ->\n  %s." x (string_of_erl_expr e)
  | EFun { name; args; expr } ->
    Printf.sprintf "%s(%s) ->\n  %s."
      name
      (String.concat ", " (List.map capitalize_first args))
      (string_of_erl_expr expr)

module M = Map.Make(String)

type ctx =
  { mutable id: int
  ; mutable top_funs: string list
  (* List of top level function, used to know when to capitalize the names or not *)
  ; mutable top_vars: string list
  (* A mapping from actual symbol to symbol with id *)
  ; mutable vars: string M.t
  }

let reset_id ctx =
  ctx.id <- 0

let next_id ctx =
  ctx.id <- ctx.id + 1;
  ctx.id

let rec comp_term ctx term =
  match term with
  | KLit (LSym s) ->
    if List.mem s ctx.top_vars then
      EApp (ELit (LSym s), [], true)
    else if M.mem s ctx.vars then
      ELit (LSym (M.find s ctx.vars))
    else
      ELit (LSym s)
  | KLit l -> ELit l
  | KBin (a, op, b) -> EBin (comp_term ctx a, op, comp_term ctx b)
  | KList l -> EList (List.map (comp_term ctx) l)
  | KTuple l -> ETuple (List.map (comp_term ctx) l)
  | KThen (a, b) -> EThen (comp_term ctx a, comp_term ctx b)
  | KApp (KLit (LSym "__external__"), xs) ->
    (match xs with
    | KLit (LStr f) :: args -> EApp (ELit (LSym f), List.map (comp_term ctx) args, true)
    | x -> List.map show_kterm x
      |> String.concat ", "
      |> (^) __LOC__
      |> (^) "Invalid external call: "
      |> failwith)
  | KApp (KLit (LSym "__inline__"), xs) ->
    (match xs with
    | [KLit (LStr c)] -> EInline c
    | x -> List.map show_kterm x
      |> String.concat ", "
      |> (^) __LOC__
      |> (^) "Invalid external call: "
      |> failwith)
  | KApp (f, xs) ->
    let is_top = match f with
      | KLit (LSym s) -> List.mem s ctx.top_funs
      | _ -> false
    in
    EApp (comp_term ctx f, List.map (comp_term ctx) xs, is_top)
  | KIf { cond; t; f } ->
    let cond = comp_term ctx cond in
    let ifsym = "_if" ^ string_of_int (next_id ctx) in
    EThen (
      ELet (ifsym, cond),
      EIf (
        EBin (ELit (LSym ifsym), Eq, ELit (LBool true)),
        comp_term ctx t,
        comp_term ctx f))
  | KDef { name; body; in_ ; _ } ->
    let sym = name ^ string_of_int (next_id ctx) in
    ctx.vars <- M.add name sym ctx.vars;
    let body = comp_term ctx body in
    EThen (
      ELet (sym, body),
      comp_term ctx in_)
  | KFun { name; args; body; in_; _ } ->
    let sym = name ^ string_of_int (next_id ctx) in
    ctx.vars <- M.add name sym ctx.vars;
    let body = comp_term ctx body in
    EThen (
      ELet (sym, ELam (sym, args, body)),
      comp_term ctx in_)
  | KDestruct { names; body; in_; } ->
    let syms = List.map (fun name -> name ^ string_of_int (next_id ctx)) names in
    List.iter2 (fun name sym -> ctx.vars <- M.add name sym ctx.vars) names syms;
    let body = comp_term ctx body in
    EThen (
      ELetD (syms, body),
      comp_term ctx in_)
  | e -> todo __LOC__ ~reason:(show_kterm e)

let comp_top ctx top =
  match top with
  | KTDef (name, body) ->
    reset_id ctx;
    ctx.top_vars <- name :: ctx.top_vars;
    EDef (name, comp_term ctx body)
  | KTFun { name; args; body; _ } ->
    reset_id ctx;
    ctx.top_funs <- name :: ctx.top_funs;
    EFun { name; args; expr = comp_term ctx body }

let comp terms =
  let ctx =
    { id = 0
    ; top_funs = []
    ; top_vars = []
    ; vars = M.empty
    }
  in
  let comped = List.map (comp_top ctx) terms in

  let exports = List.fold_left (fun acc -> function
      | EFun { name; args; _ } -> (name, List.length args) :: acc
      | _ -> acc
    ) [] comped in

  EExport exports :: comped
