open Common
open Utils
open Infer

type erl_expr =
  | ELit of lit
  | EBin of erl_expr * bin * erl_expr
  | EApp of string * erl_expr list
  (* fun (args) -> e end *)
  | ELam of string list * erl_expr
  (* e1, e2. *)
  | EThen of erl_expr * erl_expr
  (* x = e *)
  | ELet of string * erl_expr

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

let rec string_of_erl_expr = function
  | ELit l -> string_of_lit l
  | EBin (a, op, b) -> Printf.sprintf "%s %s %s" (string_of_erl_expr a) (string_of_bin op) (string_of_erl_expr b)
  | EApp (f, args) ->
    Printf.sprintf "%s(%s)" f (String.concat ", " (List.map string_of_erl_expr args))
  | ELam (args, e) ->
    Printf.sprintf "fun(%s) -> %s end" (String.concat ", " args) (string_of_erl_expr e)
  | EThen (e1, e2) ->
    Printf.sprintf "%s,\n  %s" (string_of_erl_expr e1) (string_of_erl_expr e2)
  | ELet (x, e) -> Printf.sprintf "%s = %s" x (string_of_erl_expr e)

let string_of_erl_top = function
  | EExport exports ->
    Printf.sprintf "-export([%s])."
      (String.concat ", " (List.map (fun (f, a) -> Printf.sprintf "%s/%d" f a) exports))
  | EDef (x, e) -> Printf.sprintf "%s() ->\n  %s." x (string_of_erl_expr e)
  | EFun { name; args; expr } ->
    Printf.sprintf "%s(%s) ->\n  %s."
      name
      (String.concat ", " args)
      (string_of_erl_expr expr)

let rec comp_term term =
  match fst term with
  | TLit (l, _) -> ELit l
  | TBin (a, op, b) -> EBin (comp_term a, op, comp_term b)
  | TLet { name; args = None; body; in_ ; _ } ->
    EThen(
      ELet (fst name, comp_term body),
      comp_term in_)
  | TLet { name; args = Some args; body; in_; _ } ->
    EThen(
      ELet (fst name, ELam (List.map (fun x -> fst @@ fst x) args, comp_term body)),
      comp_term in_)
  | e -> todo @@ __LOC__ ^ " " ^ show_term e

let comp_top top =
  match fst top with
  | TTLet { name; args = None; body; _ } ->
    EDef (fst name, comp_term body)
  | TTLet { name; args = Some args; body; _ } ->
    EFun
      { name = fst name
      ; args = List.map (fun x -> fst @@ fst x) args
      ; expr = comp_term body
      }

let comp terms =
  let comped = List.map comp_top terms in
  let exports = List.fold_left (fun acc -> function
      | EFun { name; args; _ } -> (name, List.length args) :: acc
      | _ -> acc
    ) [] comped in
  EExport exports :: comped
