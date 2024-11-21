open Common
open Utils
open Loc
open Parse
open Map

type term =
  | TLit   of lit spanned
  | TList  of term spanned list
  | TBin   of term spanned * bin * term spanned
  | TApp   of term spanned * term spanned
  | TThen  of term spanned * term spanned
  | TIf of
    { cond: term spanned
    ; t: term spanned
    ; f: term spanned
    }
  | TLet of
    { name: string spanned
    ; args: (string spanned * typ) list option
    ; ret: typ
    ; body: term spanned
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
    ; args: (string spanned * typ) list option
    ; ret: typ
    ; body: term spanned
    }
  [@@deriving show]

and scheme =
  | Forall of string list * typ

let empty_scheme t = Forall ([], t)

let string_of_scheme = function
  | Forall ([], t) -> string_of_typ t
  | Forall (vs, t) -> "∀ " ^ String.concat ", " vs ^ ". " ^ string_of_typ t

module Subst = Map.Make(String)

let to_scheme (s : typ Subst.t) : scheme Subst.t =
  Subst.map (fun t -> Forall ([], t)) s

let rec apply_ty (subst : scheme Subst.t) t =
  match t with
  | TyVar v -> (try (match Subst.find v subst with
    | Forall (_, t) -> apply_ty subst t) with Not_found -> t)
  | TyTuple (t1, t2) ->
    TyTuple (apply_ty subst t1, apply_ty subst t2)
  | TyArrow (t1, t2) ->
    TyArrow (apply_ty subst t1, apply_ty subst t2)
  | TyConstructor (name, t) ->
    TyConstructor (name, apply_ty subst t)
  | TyConst _ -> t

let rec apply_scheme (subst : scheme Subst.t) scheme =
  match scheme with
  | Forall (bound, ty) ->
    let ty = apply_ty subst ty in
    (* Filter out the bound variables that are in the substitution *)
    let bound = List.filter (fun v -> not (Subst.mem v subst)) bound in
    Forall (bound, apply_ty subst ty)

let compose (s1 : scheme Subst.t) (s2 : scheme Subst.t) =
  let s2_mapped = Subst.map (fun scheme -> apply_scheme s1 scheme) s2 in
  Subst.fold Subst.add s2_mapped s1

let rec occurs v t =
  match t with
  | TyVar var when v = var -> true
  | TyVar _ -> false
  | TyTuple (t1, t2) -> occurs v t1 || occurs v t2
  | TyArrow (t1, t2) -> occurs v t1 || occurs v t2
  | TyConstructor (_, t) -> occurs v t
  | TyConst _ -> false

let rec unify t u =
  let rec apply_ty subst t =
    match t with
    | TyVar v -> (try Subst.find v subst with Not_found -> t)
    | TyTuple (t1, t2) ->
      TyTuple (apply_ty subst t1, apply_ty subst t2)
    | TyArrow (t1, t2) ->
      TyArrow (apply_ty subst t1, apply_ty subst t2)
    | TyConstructor (name, t) ->
      TyConstructor (name, apply_ty subst t)
    | TyConst _ -> t
  in
  let compose s1 s2 =
    let s2_mapped = Subst.map (fun t -> apply_ty s1 t) s2 in
    Subst.fold Subst.add s2_mapped s1
  in
  match (t, u) with
  | TyConst l, TyConst r when l = r -> Ok Subst.empty
  | TyVar v, t | t, TyVar v ->
    if occurs v t then
      Error ("Infinite type: " ^ v ^ " occurs in " ^ string_of_typ t)
    else
      Ok (Subst.singleton v t)
  | TyTuple (t1, t2), TyTuple (u1, u2)
  | TyArrow (t1, t2), TyArrow (u1, u2) ->
    let* s1 = unify t1 u1 in
    let* s2 = unify (apply_ty s1 t2) (apply_ty s1 u2) in
    Ok (compose s2 s1)
  | TyConstructor (l, t), TyConstructor (r, u) when l = r ->
    unify t u
  | _ -> Error ("Cannot unify " ^ string_of_typ t ^ " with " ^ string_of_typ u)

let rec free_vars = function
  | TyVar v   -> [v]
  | TyConst _ -> []
  | TyTuple (t1, t2) -> free_vars t1 @ free_vars t2
  | TyArrow (t1, t2) -> free_vars t1 @ free_vars t2
  | TyConstructor (_, t) -> free_vars t

let free_vars_scheme = function
  | Forall (bound, ty) ->
    List.filter (fun v -> not (List.mem v bound)) (free_vars ty)

type context = scheme Subst.t

let fresh =
  let counter = ref 0 in
  fun () ->
    let id = !counter in
    counter := !counter + 1;
    TyVar ("'" ^ str_from_int id)

let generalize ctx t =
  (* Find free vars in the type that are not in the context *)
  let ctx_vars =
    List.fold_left
      (fun acc (_, scheme) ->
        match scheme with
        | Forall (bound, _) -> acc @ bound)
      [] (Subst.bindings ctx)
  in
  let free_type_vars =
    List.filter (fun v -> not (List.mem v ctx_vars)) (free_vars t)
  in
  Forall (free_type_vars, t)

let instantiate (Forall (bound, ty)) =
  let subst = List.fold_left
    (fun acc var -> Subst.add var (empty_scheme (fresh ())) acc)
    Subst.empty bound
  in
  apply_ty subst ty

let show_context ctx =
  Subst.fold (fun k v acc -> acc ^ k ^ " : " ^ string_of_scheme v ^ "\n") ctx ""
  |> String.trim

let rec infer_expr (ctx : scheme Subst.t) e =
  let oks x ty subst = Ok ((x, snd e), ty, subst) in
  match (fst e) with
  | CLit (LSym s, span) ->
    (match Subst.find_opt s ctx with
    | Some scheme ->
      oks (TLit (LSym s, span)) (instantiate scheme) Subst.empty
    | None -> Error ("Unbound variable " ^ s, span))

  | CLit (LUnit, s)    -> oks (TLit (LUnit, s))    (TyConst "unit")  Subst.empty
  | CLit (LBool x, s)  -> oks (TLit (LBool x, s))  (TyConst "bool")  Subst.empty
  | CLit (LInt x, s)   -> oks (TLit (LInt x, s))   (TyConst "int")   Subst.empty
  | CLit (LFloat x, s) -> oks (TLit (LFloat x, s)) (TyConst "float") Subst.empty

  | CBin (a, op , b) ->
    let* (a, a_ty, a_s) = infer_expr ctx a in
    let* (b, b_ty, b_s) = infer_expr ctx b in

    (* Expected type *)
    let expect_args_ty = match op with
      | Add | Sub | Mul | Div | Mod ->
        TyConst "int"
      | Eq | Neq | Lt | Lte | Gt | Gte ->
        fresh ()
      | And | Or ->
        TyConst "bool"
    in
    let ret_ty = match op with
      | Add | Sub | Mul | Div | Mod ->
        TyConst "int"
      | Eq | Neq | Lt | Lte | Gt | Gte
      | And | Or ->
        TyConst "bool"
    in

    (* Unify them to have the operator's expected type *)
    let* unify_a_s =
      unify (apply_ty a_s a_ty) expect_args_ty
      |> Result.map to_scheme
      |> Result.map_error (fun err -> (err, snd a))
    in
    let* unify_b_s =
      unify (apply_ty b_s b_ty) expect_args_ty
      |> Result.map to_scheme
      |> Result.map_error (fun err -> (err, snd b))
    in

    oks (TBin (a, op, b))
      (* (apply_ty unify_b_s expect_args_ty) *)
      ret_ty
      (a_s |> compose b_s |> compose unify_a_s |> compose unify_b_s)

  | CThen (a, b) ->
    let* (a, _a_ty, a_s) = infer_expr ctx a in
    let* (b, b_ty, b_s) = infer_expr ctx b in
    oks (TThen (a, b))
      b_ty
      (compose a_s b_s)

  | CApp (f, x) ->
    let* (f, f_ty, fs) = infer_expr ctx f in
    let* (x, x_ty, xs) = infer_expr ctx x in
    let res_ty = fresh () in
    let* unified_subst =
      unify (apply_ty xs f_ty) (TyArrow (x_ty, res_ty))
      |> Result.map to_scheme
      |> Result.map_error (fun err -> (err, snd f))
    in
    oks (TApp (f, x))
      (apply_ty unified_subst res_ty)
      (compose unified_subst (compose fs xs))

  | CLet { name; args = None; body; ret; in_ } ->

    let ret = Option.value ret ~default:(fresh ()) in
    let* (b, b_ty, bs) = infer_expr ctx body in
    let gen_b_ty = generalize ctx (apply_ty bs b_ty) in

    let in_ctx = Subst.add (fst name) gen_b_ty ctx in

    let* (in_, in_ty, in_s) = infer_expr in_ctx in_ in

    let* ret_ty_s =
      unify ret in_ty
      |> Result.map to_scheme
      |> Result.map_error (fun err -> (err, snd body))
    in
    let ret = apply_ty ret_ty_s ret in

    oks (TLet
      { name
      ; args = None
      ; ret
      ; body = b
      ; in_ })
      in_ty
      (compose bs in_s)

  | CLet { name; args = Some args; body; ret; in_ } ->

    (* Just the args *)
    let args_name = List.map (fun x -> fst x) args in
    (* Fresh types or the argument's type hints *)
    let args_ty = List.map (fun (_, t) -> Option.value t ~default:(fresh ())) args in
    (* Return type *)
    let ret = Option.value ret ~default:(fresh ()) in

    (* Make the function type *)
    let rec make_ft = function
      | [] -> ret
      | arg :: rest ->
        TyArrow (arg, make_ft rest)
    in
    let f_ty = make_ft args_ty in

    (* Add arguments into body context *)
    let args_scheme = List.combine
      (List.map (fun x -> fst x) args_name)
      (List.map (fun t -> Forall ([], t)) args_ty)
    in
    let body_ctx = List.fold_left
      (fun subst (name, ty) -> Subst.add name ty subst)
      ctx args_scheme
    in

    (* Infer body *)
    let* (b, b_ty, bs) = infer_expr body_ctx body in

    let args_ty = List.map (fun x -> apply_ty bs x) args_ty in

    (* Unifies that the function type match the body type *)
    let* f_ty_s =
      unify ret b_ty
      |> Result.map to_scheme
      |> Result.map_error (fun err -> (err, snd body))
    in
    let ret = apply_ty f_ty_s ret in

    (* Generalize function type *)
    let gen_f_ty =
      apply_ty bs f_ty
      |> generalize body_ctx
      |> apply_scheme f_ty_s in

    (* Add function to context *)
    let ctx = Subst.add (fst name) gen_f_ty ctx in

    (* Infer `in`'s expression *)
    let* (in_, in_ty, in_s) = infer_expr ctx in_ in

    let args = List.combine args_name args_ty in
    oks (TLet
        { name
        ; args = Some args
        ; ret
        ; body = b
        ; in_ })
      in_ty
      (compose bs (compose f_ty_s in_s))

  | e -> todo @@ __LOC__ ^ " " ^ show_cst e

let infer_top (ctx : scheme Subst.t ref) e =
  let oks x = Ok (x, snd e) in
  match fst e with
  | CTLet { name; body; args = None; ret } ->
    let ret = Option.value ret ~default:(fresh ()) in
    let* (b, b_ty, _bs) = infer_expr !ctx body in

    let* ret_ty_s =
      (* We probably don't need to `apply_ty bs b_ty` :clueless: *)
      unify ret b_ty
      |> Result.map to_scheme
      |> Result.map_error (fun err -> (err, snd body))
    in

    let ret = apply_ty ret_ty_s ret in
    oks (TTLet
      { name
      ; args = None
      ; ret
      ; body = b })

  | CTLet { name; body; args = Some args; ret } ->
    let args_name = List.map (fun x -> fst x) args in
    let args_ty = List.map (fun (_, t) -> Option.value t ~default:(fresh ())) args in
    let ret = Option.value ret ~default:(fresh ()) in

    let rec make_ft = function
      | [] -> ret
      | arg :: rest ->
        TyArrow (arg, make_ft rest)
    in
    let f_ty = make_ft args_ty in

    let args_scheme = List.combine
      (List.map (fun x -> fst x) args_name)
      (List.map (fun t -> Forall ([], t)) args_ty)
    in
    let body_ctx = List.fold_left
      (fun subst (name, ty) -> Subst.add name ty subst)
      !ctx args_scheme
    in

    let* (b, b_ty, bs) = infer_expr body_ctx body in

    let args_ty = List.map (fun x -> apply_ty bs x) args_ty in

    let* f_ty_s =
      unify ret b_ty
      |> Result.map to_scheme
      |> Result.map_error (fun err -> (err, snd body))
    in
    let ret = apply_ty f_ty_s ret in

    let gen_f_ty =
      apply_ty bs f_ty
      |> generalize body_ctx
      |> apply_scheme f_ty_s in

    ctx := Subst.add (fst name) gen_f_ty !ctx;

    let args = List.combine args_name args_ty in
    oks (TTLet
        { name
        ; args = Some args
        ; ret
        ; body = b })

let infer es =
  let ctx = ref Subst.empty in
  map_sep_results @@ List.map (infer_top ctx) es
