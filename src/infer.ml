open Common
open Utils
open Loc
open Parse

type term =
  | TLit   of lit spanned
  | TList  of term spanned list
  | TTuple of term spanned list
  | TRecord of (string spanned * term spanned) list
  | TAccess of term spanned * string spanned
  | TBin   of term spanned * bin * term spanned
  | TApp   of term spanned * term spanned
  | TThen  of term spanned * term spanned
  | TLambda of
    { args: (string spanned * typ) list
    ; ret: typ
    ; body: term spanned
    }
  | TIf of
    { cond: term spanned
    ; t: term spanned
    ; f: term spanned
    }
  | TDef of
    { name: string spanned
    ; body: term spanned
    ; typ: typ
    ; in_: term spanned
    }
  | TFun of
    { name: string spanned
    ; args: (string spanned * typ) list
    ; ret: typ
    ; recr: bool
    ; body: term spanned
    ; in_: term spanned
    }
  | TDestruct of
    { names: string spanned list
    ; types: typ list
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
  | TTDef of
    { name: string spanned
    ; body: term spanned
    ; ret: typ
    }
  | TTFun of
    { name: string spanned
    ; args: (string spanned * typ) list
    ; ret: typ
    ; recr: bool
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

type context =
  { mutable env: scheme Subst.t
  ; mutable typedefs: scheme Subst.t
  (* List of auto generatedtype constructors function *)
  ; mutable typeconstrs: string list
  }

let show_context ctx =
  Subst.fold (fun k v acc -> acc ^ k ^ " : " ^ string_of_scheme v ^ "\n") ctx ""
  |> String.trim

let ctx_define ctx name scheme =
  ctx.env <- Subst.add name scheme ctx.env

let ctx_define_type ctx name quantified typ =
  ctx.typedefs <- Subst.add name (Forall (quantified, typ)) ctx.typedefs

let fresh =
  let counter = ref 0 in
  fun () ->
    let id = !counter in
    counter := !counter + 1;
    TyInfer (string_of_int id)

let to_scheme (s : typ Subst.t) : scheme Subst.t =
  Subst.map (fun t -> Forall ([], t)) s

let rec apply_ty (subst : scheme Subst.t) t =
  match t with
  | TyInfer v -> (try (match Subst.find v subst with
    (* TODO i dont know if i have to apply_ty here because sometimes it overflow :( *)
    | Forall (_, t) -> t) with Not_found -> t)
  | TyTuple (t1, t2) ->
    TyTuple (apply_ty subst t1, apply_ty subst t2)
  | TyArrow (t1, t2) ->
    TyArrow (apply_ty subst t1, apply_ty subst t2)
  | TyRecord fields ->
    TyRecord (List.map (fun (name, t) -> (name, apply_ty subst t)) fields)
  | TyEnum variants ->
    TyEnum (List.map (fun (name, ts) -> (name, Option.map (apply_ty subst) ts)) variants)
  | TyConstructor (name, t) ->
    TyConstructor (name, apply_ty subst t)
  | TyConst _ -> t

let instantiate (Forall (bound, ty)) =
  let subst = List.fold_left
    (fun acc var -> Subst.add var (empty_scheme (fresh ())) acc)
    Subst.empty bound
  in
  apply_ty subst ty

let apply_scheme (subst : scheme Subst.t) scheme =
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
  | TyInfer var when v = var -> true
  | TyInfer _ -> false
  | TyTuple (t1, t2) -> occurs v t1 || occurs v t2
  | TyArrow (t1, t2) -> occurs v t1 || occurs v t2
  | TyRecord fields -> List.exists (fun (_, t) -> occurs v t) fields
  | TyEnum variants -> List.exists (fun (_, t) ->
    if Option.is_some t then occurs v (Option.get t) else false) variants
  | TyConstructor (_, t) -> occurs v t
  | TyConst _ -> false

let rec unify ?(context="") (ctx: context) t u =
  let rec apply_ty0 subst t =
    match t with
    | TyInfer v -> (try Subst.find v subst with Not_found -> t)
    | TyTuple (t1, t2) ->
      TyTuple (apply_ty0 subst t1, apply_ty0 subst t2)
    | TyArrow (t1, t2) ->
      TyArrow (apply_ty0 subst t1, apply_ty0 subst t2)
    | TyRecord fields ->
      TyRecord (List.map (fun (name, t) -> (name, apply_ty0 subst t)) fields)
    | TyEnum variants ->
      TyEnum (List.map (fun (name, ts) -> (name, Option.map (apply_ty0 subst) ts)) variants)
    | TyConstructor (name, t) ->
      TyConstructor (name, apply_ty0 subst t)
    | TyConst _ -> t
  in
  let compose s1 s2 =
    let s2_mapped = Subst.map (fun t -> apply_ty0 s1 t) s2 in
    Subst.fold Subst.add s2_mapped s1
  in
  let make_err t u =
    let ts = floor_types [t; u] in
    Printf.sprintf "Expected type %s does not match %s%s"
    (string_of_typ (List.hd ts)) (string_of_typ (List.hd (List.tl ts)))
    (if context = "" then "" else " in " ^ context) in
  match (t, u) with
  | TyInfer v, t | t, TyInfer v ->
    if t = TyInfer v then
      Ok Subst.empty
    else if occurs v t then
      Error ("Recursive type: `" ^ v ^ " occurs in " ^ string_of_typ t)
    else
      Ok (Subst.singleton v t)

  | TyConst l, TyConst r when l = r && List.mem l intrinsic_types -> Ok Subst.empty
  | TyConst l, TyConst r when List.mem l intrinsic_types && List.mem r intrinsic_types ->
    Error (make_err t u)
  | TyConst l, v | v, TyConst l ->
    if List.mem l intrinsic_types then
      match v with
      | TyConst r ->
        (match Subst.find_opt r ctx.typedefs with
        | Some scheme ->
          unify ctx (TyConst l) (instantiate scheme) ~context
        | None -> Error (Printf.sprintf "Unbound type %s%s" l
          (if context = "" then "" else " in " ^ context)))
      | _ -> Error (make_err t u)
    else
      (match Subst.find_opt l ctx.typedefs with
      | Some scheme ->
        unify ctx (instantiate scheme) v ~context
      | None -> Error (Printf.sprintf "Unbound type %s%s" l
        (if context = "" then "" else " in " ^ context)))

  | TyTuple (t1, t2), TyTuple (u1, u2)
  | TyArrow (t1, t2), TyArrow (u1, u2) ->
    let* s1 = unify ctx t1 u1 ~context in
    let* s2 = unify ctx (apply_ty0 s1 t2) (apply_ty0 s1 u2) ~context in
    Ok (compose s2 s1)
  | TyRecord fields1, TyRecord fields2 when List.length fields1 = List.length fields2 ->
    List.fold_left2
      (fun acc (_l1, t1) (_l2, t2) ->
        let* s = acc in
        let* s1 = unify ctx t1 t2 ~context:("type " ^ string_of_typ t) in
        Ok (compose s1 s))
      (Ok Subst.empty) fields1 fields2
  | TyConstructor (l, t), TyConstructor (r, u) when l = r ->
    unify ctx t u ~context
  | _ -> Error (make_err t u)

let convert_comma_user s =
  if s.[0] = '\'' then "user" ^ s else s

let rec convert_comma_const_ty = function
  (* Insert 'u' in front to differentiate between types generated by inferrence and
     user defined forall bound types *)
  | TyConst t when t.[0] = '\'' -> TyInfer ("user" ^ t)
  | TyConst _ as t -> t
  | TyInfer _ as t -> t
  | TyTuple (a, b) ->
    TyTuple (convert_comma_const_ty a, convert_comma_const_ty b)
  | TyArrow (a, b) -> TyArrow
    (convert_comma_const_ty a, convert_comma_const_ty b)
  (* TODO not sure about this in case of `enum Foo 'a = | Foo 'a ...`
     and vice versa for records *)
  | TyRecord fields -> TyRecord fields
  | TyEnum variants -> TyEnum variants
  | TyConstructor (name, t) ->
    TyConstructor (name, convert_comma_const_ty t)

let rec free_vars = function
  | TyInfer v   -> [v]
  | TyConst _ -> []
  | TyTuple (t1, t2) -> free_vars t1 @ free_vars t2
  | TyArrow (t1, t2) -> free_vars t1 @ free_vars t2
  | TyRecord fields -> List.fold_left (fun acc (_, t) -> acc @ free_vars t) [] fields
  | TyEnum variants -> List.fold_left (fun acc (_, t) ->
    if Option.is_some t then acc @ free_vars (Option.get t) else acc) [] variants
  | TyConstructor (_, t) -> free_vars t

let free_vars_scheme = function
  | Forall (bound, ty) ->
    List.filter (fun v -> not (List.mem v bound)) (free_vars ty)

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

let unify_err ?(context="") ?(hint="") ctx t u where =
  unify ctx t u ~context
  |> Result.map to_scheme
  |> Result.map_error (fun m -> err m where ~hint)

let or_fresh = function
  | Some t -> fst t
  | None -> fresh ()

let rec infer_expr (ctx: context) e =
  let unify_err ?(context="") ?(hint="") t u where = unify_err ~context ~hint ctx t u where in
  let oks x ty subst = Ok ((x, snd e), ty, subst) in
  match (fst e) with
  | CLit (LSym s, span) ->
    (match Subst.find_opt s ctx.env with
    | Some scheme ->
      oks (TLit (LSym s, span)) (instantiate scheme) Subst.empty
    | None -> err_ret ("Unbound variable " ^ s) span)

  | CLit (LUnit, s)    -> oks (TLit (LUnit, s))    (TyConst "unit")   Subst.empty
  | CLit (LBool x, s)  -> oks (TLit (LBool x, s))  (TyConst "bool")   Subst.empty
  | CLit (LInt x, s)   -> oks (TLit (LInt x, s))   (TyConst "int")    Subst.empty
  | CLit (LFloat x, s) -> oks (TLit (LFloat x, s)) (TyConst "float")  Subst.empty
  | CLit (LStr x, s)   -> oks (TLit (LStr x, s))   (TyConst "string") Subst.empty

  | CList [] -> oks (TList []) (TyConstructor ("list", fresh ())) Subst.empty
  | CList xs ->
      let* xs' = map_early_return (infer_expr ctx) xs in
      let (_, expected_t, s) = List.hd xs' in
      let* unified_subst = List.fold_left
        (fun acc (x, t, t_s) ->
          let* s = unify_err (apply_ty t_s t) expected_t (snd x) ~context:"list" in
          acc |> Result.map (fun acc -> compose acc s))
        (Ok s) (List.tl xs')
      in
      let xs = List.map (fun (x, _ ,_) -> x) xs' in
      let t = (apply_ty unified_subst expected_t)
        |> (fun t -> TyConstructor ("list", t))
      in
      oks (TList xs) t unified_subst

  | CTuple xs ->
    let* xs' = map_early_return (infer_expr ctx) xs in
    let (_, t, _) = List.hd xs' in
    let t = List.fold_right
      (fun (_, u, _) acc -> TyTuple (acc, u))
      (List.tl xs') t
    in
    let xs = List.map (fun (x, _, _) -> x) xs' in
    let subst = List.fold_left
      (fun acc (_, _, s) -> compose acc s)
      Subst.empty xs'
    in
    oks (TTuple xs) t subst

  | CRecord fields ->
    let* fields' = map_early_return (fun (name, expr) ->
      let* (expr, t, s) = infer_expr ctx expr in
      Ok((name, expr, t, s))) fields in
    let fields = List.map (fun (name, term, _, _) -> (name, term)) fields' in
    let t = TyRecord (List.map (fun (name, _, t, _) -> (fst name, t)) fields') in
    let subst = List.fold_left
      (fun acc (_, _, _, s) -> compose acc s)
      Subst.empty fields'
    in
    oks (TRecord fields) t subst

  | CAccess (record, field) ->
    let* (record, record_ty, record_s) = infer_expr ctx record in
    (* Get the type of that field *)
    (match record_ty with
    | TyRecord fields ->
      let field_ty = List.assoc (fst field) fields in
      oks (TAccess (record, field)) field_ty record_s
    | _ -> err_ret ("Expected record type, got " ^ string_of_typ record_ty) (snd record))

  | CBin (a, op , b) ->
    let* (a, a_ty, a_s) = infer_expr ctx a in
    let* (b, b_ty, b_s) = infer_expr ctx b in

    (* Expected type *)
    let expected_lhs_ty = match op with
      | Add | Sub | Mul | Div | Mod -> TyConst "int"
      | Eq | Neq | Lt | Lte | Gt | Gte -> a_ty
      | And | Or -> TyConst "bool"
      | Cons -> a_ty
    in
    let expected_rhs_ty = match op with
      | Cons -> TyConstructor ("list", expected_lhs_ty)
      | _ -> expected_lhs_ty
    in
    let ret_ty = match op with
      | Add | Sub | Mul | Div | Mod -> TyConst "int"
      | Eq | Neq | Lt | Lte | Gt | Gte
      | And | Or -> TyConst "bool"
      | Cons -> TyConstructor ("list", expected_lhs_ty)
    in

    (* Unify them to have the operator's expected type *)
    let* unify_a_s =
      unify_err a_ty expected_lhs_ty (snd a)
      ~context:"left operand" in
    let* unify_b_s =
      unify_err (apply_ty unify_a_s expected_rhs_ty) b_ty (snd b)
      ~context:"right operand" in

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
    let hint = match (f_ty, x_ty) with
      | (TyArrow _, _) | (_, TyArrow _) -> ""
      (* If the left hand side is already a non-arrow type
         and right hand side is still expecting it to be arrow
         then there's probably too much arguments provided *)
      | _ -> (match fst f with
        | TApp _ -> "Maybe too many arguments provided"
        | _ -> "Maybe this can't be applied") in
    let* unified_subst =
      unify_err f_ty (TyArrow (x_ty, res_ty)) (snd x)
      ~context:(
        (* Check if f is a type consstructor function *)
        match fst f with
        | TLit (LSym f, _) -> (match List.mem f ctx.typeconstrs with
          | true -> Printf.sprintf "type constructor %s" f
          | false -> "function application")
        | _ -> "function application"
      )
      ~hint:hint
    in
    oks (TApp (f, x))
      (apply_ty unified_subst res_ty)
      (compose unified_subst (compose fs xs))

  | CLambda { args; ret; body } ->
    let args_name = List.map (fun x -> fst x) args in
    let args_ty = List.map (fun (_, t) -> or_fresh t) args in
    let ret = or_fresh ret in

    let rec make_ft = function
      | [] -> ret
      | arg :: rest -> TyArrow (arg, make_ft rest)
    in
    let f_ty = make_ft args_ty in

    let args_scheme = List.combine
      (List.map (fun x -> fst x) args_name)
      (List.map (fun t -> Forall ([], t)) args_ty)
    in
    let body_env = List.fold_left
      (fun subst (name, ty) -> Subst.add name ty subst)
      ctx.env args_scheme
    in

    let body_ctx = ref { ctx with env = body_env } in
    let* (b, b_ty, bs) = infer_expr !body_ctx body in

    let args_ty = List.map (fun x -> apply_ty bs x) args_ty in

    let* ret_ty_s = unify_err ret b_ty (snd body) ~context:"lambda" in
    let ret = apply_ty ret_ty_s ret in
    let f_ty = apply_ty (compose bs ret_ty_s) f_ty in

    let args = List.combine args_name args_ty in
    oks (TLambda
        { args
        ; ret
        ; body = b })
      f_ty
      (compose bs ret_ty_s)

  | CIf { cond; t; f } ->
    let* (cond, cond_ty, cond_s) = infer_expr ctx cond in
    let* (t, t_ty, t_s) = infer_expr ctx t in
    let* (f, f_ty, f_s) = infer_expr ctx f in

    let* unify_cond_s = unify_err cond_ty (TyConst "bool") (snd cond) ~context:"if condition" in
    let ret_ty = fresh () in
    let* unify_t_s = unify_err t_ty ret_ty (snd t) ~context:"return type of true branch" in
    let* unify_f_s = unify_err (apply_ty unify_t_s ret_ty) f_ty (snd f) ~context:"return type of false branch" in

    oks (TIf { cond; t; f })
      (apply_ty unify_f_s t_ty)
      ((compose t_s f_s)
      |> compose cond_s
      |> compose unify_t_s
      |> compose unify_f_s
      |> compose unify_cond_s)

  | CDef { name; body; typ; in_ } ->
    let typ = or_fresh typ in
    let* (b, b_ty, bs) = infer_expr ctx body in

    let* ty_s = unify_err typ (apply_ty bs b_ty) (snd body) ~context:"definition" in
    let typ = apply_ty ty_s typ in

    let gen_typ = generalize ctx.env (apply_ty ty_s typ) in
    let in_env = Subst.add (fst name) gen_typ ctx.env in

    let in_ctx = ref { ctx with env = in_env } in
    let* (in_, in_ty, in_s) = infer_expr !in_ctx in_ in

    oks (TDef
      { name
      ; body = b
      ; typ
      ; in_ })
      in_ty
      (compose bs in_s)

  | CFun { name; args ; body; ret; recr; in_; } ->
    (* Just the args *)
    let args_name = List.map (fun x -> fst x) args in
    (* Fresh types or the argument's type hints *)
    let args_ty = List.map (fun (_, t) -> or_fresh t |> convert_comma_const_ty) args in
    (* Return type *)
    let ret = or_fresh ret |> convert_comma_const_ty in

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
    let body_env = List.fold_left
      (fun subst (name, ty) -> Subst.add name ty subst)
      ctx.env args_scheme
    in
    let body_env = if recr then
      Subst.add (fst name) (empty_scheme f_ty) body_env
    else body_env in

    (* Infer body *)
    let body_ctx = ref { ctx with env = body_env } in
    let* (b, b_ty, bs) = infer_expr !body_ctx body in

    let args_ty = List.map (fun x -> apply_ty bs x) args_ty in

    (* Unifies that the function's return type match the body type *)
    let* ret_ty_s = unify_err ret b_ty (snd body) ~context:"function body" in
    let ret = apply_ty ret_ty_s ret in

    (* Generalize function type *)
    let gen_f_ty =
      apply_ty bs f_ty
      |> generalize !body_ctx.env
      |> apply_scheme ret_ty_s in

    (* Add function to context *)
    let in_env = Subst.add (fst name) gen_f_ty ctx.env in

    (* Infer `in`'s expression *)
    let in_ctx = ref { ctx with env = in_env } in
    let* (in_, in_ty, in_s) = infer_expr !in_ctx in_ in

    let args = List.combine args_name args_ty in
    oks (TFun
        { name
        ; args
        ; ret
        ; recr
        ; body = b
        ; in_ })
      in_ty
      (compose bs (compose ret_ty_s in_s))

  (* | CDestruct { names; body; in_ } -> *)
  | CDestruct _ ->
    todo @@ __LOC__ ^ " CDestruct"

  | e -> todo @@ __LOC__ ^ " " ^ show_cst e

let infer_top (ctx: context ref) e =
  let unify_err ?(context="") ?(hint="") t u where = unify_err ~context ~hint !ctx t u where in
  let oks x = Ok (Some (x, snd e)) in
  match fst e with
  | CTUse _ -> Ok (None)
  | CTDef { name; body; ret } ->
    let ret = or_fresh ret in
    let* (b, b_ty, _bs) = infer_expr !ctx body in

    (* We probably don't need to `apply_ty bs b_ty` :clueless: *)
    let* ret_ty_s = unify_err ret b_ty (snd body) ~context:"definition" in

    let gen_b_ty =
      b_ty
      |> generalize !ctx.env
      |> apply_scheme ret_ty_s
    in

    ctx_define !ctx (fst name) gen_b_ty;

    let ret = apply_ty ret_ty_s ret in
    oks (TTDef
      { name
      ; ret
      ; body = b })

  | CTFun { name; body; args; ret; recr } ->
    let args_name = List.map (fun x -> fst x) args in
    let args_ty = List.map (fun (_, t) -> or_fresh t |> convert_comma_const_ty) args in
    let ret = or_fresh ret |> convert_comma_const_ty in

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
    let body_env = List.fold_left
      (fun subst (name, ty) -> Subst.add name ty subst)
      !ctx.env args_scheme
    in
    let body_env = if recr then
      Subst.add (fst name) (empty_scheme f_ty) body_env
    else body_env in

    let body_ctx = ref { !ctx with env = body_env } in
    let* (b, b_ty, bs) = infer_expr !body_ctx body in

    let args_ty = List.map (fun x -> apply_ty bs x) args_ty in

    let* f_ret_ty_s = unify_err ret b_ty (snd b) ~context:"function body" in
    let ret = apply_ty f_ret_ty_s ret in

    let gen_f_ty =
      apply_ty bs f_ty
      |> generalize !body_ctx.env
      |> apply_scheme f_ret_ty_s in

    ctx_define !ctx (fst name) gen_f_ty;

    let args = List.combine args_name args_ty in
    oks (TTFun
        { name
        ; args
        ; ret
        ; recr
        ; body = b })

  | CTType { name; quantified; typ } ->
    let typ = fst typ |> convert_comma_const_ty in
    (* 'a -> user'a so user types don't clash with inferred typevars *)
    let quantified = List.map (fun x -> fst x |> convert_comma_user) quantified in
    ctx_define_type !ctx (fst name) quantified typ;

    (* Add a type construtor function *)
    (* e.g.
       type Foo = int
       let _ = Foo 1 *)
    (match typ with
    | TyConst _ | TyTuple _ ->
      let t = TyArrow (typ, typ) in
      ctx_define !ctx (fst name) (Forall (quantified, t));
      !ctx.typeconstrs <- (fst name) :: !ctx.typeconstrs;

      oks(TTFun
        { name
        ; args = [(("__t__", snd name), typ)]
        ; ret = typ
        ; recr = false
        ; body = (TLit (LSym "__t__", snd name), snd name)
        })
    | _ ->
      print_endline @@ "TODO Type constructor fuction for " ^ string_of_typ typ;
      Ok (None))

let magic =
  (* __js__ ext_fun [args] => string -> any list -> any *)
  [ "__js__", Forall (["_1"; "_2"],
    TyArrow (TyConst "string", TyArrow (TyConstructor ("list", TyInfer "_1"), TyInfer "_2")))
  (* __inline__ string *)
  ; "__inline__", Forall (["_"], TyArrow (TyConst "string", TyInfer "_"))
  (* __js_method__ m field [args] => any -> string -> any list -> any *)
  ; "__js_method__", Forall (["_1"; "_2"; "_3"],
    TyArrow (TyInfer "_1", TyArrow (TyConst "string",
      TyArrow (TyConstructor ("list", TyInfer "_2"), TyInfer "_3"))))
  (* __js_field__ m field => any -> string -> any *)
  ; "__js_field__", Forall (["_1"; "_2"], TyArrow (TyInfer "_1", TyArrow (TyConst "string", TyInfer "_2")))
  ]

let infer es =
  let env = List.fold_left
    (fun env (name, scheme) -> Subst.add name scheme env)
    Subst.empty magic in
  let ctx = ref { env; typedefs = Subst.empty; typeconstrs = [] } in
  let (res, err) = map_sep_results @@ List.map (infer_top ctx) es in
  let res = List.filter_map (fun x -> x) res in
  (* res |> List.iter (fun (t, _) -> print_endline @@ show_term_top t); *)
  (* Subst.iter (fun k v -> print_endline @@ k ^ " : " ^ string_of_scheme v) !ctx; *)
  (res, err)
