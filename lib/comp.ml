open Common
open Utils
open Norm

type target_expr =
  | TrLit of lit
  | TrList of target_expr list
  | TrBin of target_expr * bin * target_expr
  | TrApp of target_expr * target_expr list
  (* fun name(args) -> e end *)
  | TrLam of string list * target_expr
  (* e1, e2. *)
  | TrThen of target_expr * target_expr
  (* x = e *)
  | TrLet of string * target_expr
  | TrIf of target_expr * target_expr * target_expr
  | TrRet of target_expr
  [@@deriving show]

and target_top =
  | TrDef of string * target_expr
  (* fun(args) -> e. *)
  | TrFun of
    { name: string
    ; args: string list
    ; expr: target_expr
    }
  [@@deriving show]

let rec string_of_target_expr = function
  | TrLit l -> string_of_lit l
  | TrList l -> Printf.sprintf "[%s]" (String.concat ", " (List.map string_of_target_expr l))
  | TrBin (a, Cons, b) ->
    Printf.sprintf "[%s, ...%s]"
      (string_of_target_expr a)
      (string_of_target_expr b)
  | TrBin (a, op, b) ->
    Printf.sprintf "(%s %s %s)"
      (string_of_target_expr a)
      (string_of_bin op)
      (string_of_target_expr b)
  | TrApp (f, args) ->
    Printf.sprintf "%s(%s)"
      (string_of_target_expr f)
      (String.concat ", " (List.map string_of_target_expr args))
  | TrLam (args, e) ->
    Printf.sprintf "(%s) => { %s }"
      (String.concat ", " args)
      (string_of_target_expr e)
  | TrThen (e1, e2) ->
    Printf.sprintf "%s; %s" (string_of_target_expr e1) (string_of_target_expr e2)
  | TrLet (x, e) -> Printf.sprintf "const %s = %s" x (string_of_target_expr e)
  | TrIf (e, t, f) ->
    Printf.sprintf "(%s) ? %s : %s"
      (string_of_target_expr e)
      (string_of_target_expr t)
      (string_of_target_expr f)
  | TrRet e -> Printf.sprintf "return %s" (string_of_target_expr e)

let string_of_target_top = function
  | TrDef (x, e) -> Printf.sprintf "const %s = %s;" x (string_of_target_expr e)
  | TrFun { name; args; expr } ->
    Printf.sprintf "const %s = (%s) => { %s };"
      name
      (String.concat ", " (List.map capitalize_first args))
      (string_of_target_expr expr)

module M = Map.Make(String)

type ctx =
  { mutable id: int
  (* List of top level function, used to know when to capitalize the names or not *)
  ; mutable top_funcs: string list
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
    if List.mem s ctx.top_funcs then
      TrApp (TrLit (LSym s), [])
    else if M.mem s ctx.vars then
      TrLit (LSym (M.find s ctx.vars))
    else
      TrLit (LSym s)
  | KLit l -> TrLit l
  | KBin (a, op, b) -> TrBin (comp_term ctx a, op, comp_term ctx b)
  | KList l -> TrList (List.map (comp_term ctx) l)
  | KThen (a, b) -> TrThen (comp_term ctx a, comp_term ctx b)
  | KApp (KLit (LSym "__external__"), xs) ->
    (match xs with
    | KLit (LStr f) :: args -> TrApp (TrLit (LSym f), List.map (comp_term ctx) args)
    | x -> List.map show_kterm x
      |> String.concat ", "
      |> (^) __LOC__
      |> (^) "Invalid external call: "
      |> failwith)
  | KApp (f, xs) ->
    TrApp (comp_term ctx f, List.map (comp_term ctx) xs)
  | KIf { cond; t; f } ->
    let cond = comp_term ctx cond in
    let ifsym = "_if" ^ string_of_int (next_id ctx) in
    TrThen (
      TrLet (ifsym, cond),
      TrIf (
        TrBin (TrLit (LSym ifsym), Eq, TrLit (LBool true)),
        comp_term ctx t,
        comp_term ctx f))
  | KDef { name; body; in_ ; _ } ->
    let sym = name ^ string_of_int (next_id ctx) in
    ctx.vars <- M.add name sym ctx.vars;
    let body = comp_term ctx body in
    TrThen (
      TrLet (sym, body),
      comp_term ctx in_)
  | KFun { name; args; body; in_; _ } ->
    let sym = name ^ string_of_int (next_id ctx) in
    ctx.vars <- M.add name sym ctx.vars;
    let body = comp_term ctx body in
    TrThen (
      TrLet (sym, TrLam (args, body)),
      comp_term ctx in_)
  | e -> todo __LOC__ ~reason:(show_kterm e)

let rec explicit_ret t =
  match t with
  | TrThen (l, r) ->
    (match r with
    | TrThen _ -> TrThen (explicit_ret l, explicit_ret r)
    | _ -> TrThen (explicit_ret l, TrRet (explicit_ret r)))

  | TrLam (xs, e) ->
    (match e with
    | TrThen _ -> TrLam (xs, explicit_ret e)
    | _ -> TrLam (xs, TrRet (explicit_ret e)))
  | TrLet (x, e) -> TrLet (x, explicit_ret e)

  | t -> t

let comp_top ctx top =
  match top with
  | KTDef (name, body) ->
    reset_id ctx;
    ctx.top_funcs <- name :: ctx.top_funcs;
    TrDef (name, comp_term ctx body |> explicit_ret)
  | KTFun { name; args; body; _ } ->
    reset_id ctx;
    ctx.top_funcs <- name :: ctx.top_funcs;
    TrFun { name; args; expr = comp_term ctx body |> explicit_ret }

let comp terms =
  let ctx = { id = 0; top_funcs = []; vars = M.empty } in
  List.map (comp_top ctx) terms
  |> List.map string_of_target_top
  |> String.concat "\n"
  |> Printf.sprintf "%s\nmain(process.argv.slice(2))\n"
