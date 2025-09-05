open Common
open Parse
open Types

type ast =
  | TValue  of value
  | TSym    of string
  | TApp    of ast spanned * ast spanned
  | TBin    of ast spanned * bin * ast spanned
  | TLambda of string spanned * ast spanned
  | TCase of
    { value: ast spanned
    ; br: (value spanned * ast spanned) list
    ; default_br: ast spanned
    }
  | TLet of
    { name: string spanned
    ; typ: tp
    ; value: ast spanned
    ; body: ast spanned
    }
  | TThen of ast spanned * ast spanned
  [@@deriving show]

type top =
  | TTDef of string spanned * tp * ast spanned
  [@@deriving show]

type infer =
  { mutable bindings: (string, scheme) Hashtbl.t
  ; mutable ctx: ctx
  }

let infer_new () =
  { bindings = Hashtbl.create 16
  ; ctx = { subst = IntMap.empty; next = 0 }
  }
let infer_fresh inf = ctx_new_var inf.ctx
let infer_get_scheme inf name = Hashtbl.find_opt inf.bindings name
let infer_set_scheme inf name sch = Hashtbl.add inf.bindings name sch
let infer_unset_scheme inf name = Hashtbl.remove inf.bindings name

let unify ctx t u span =
  match ctx_unify ctx t u with
  | Ok _    -> Ok ()
  | Error e -> err_ret (unify_error_to_string e) span

let rec infer_cst inf cst =
  let (cst, span) = cst in
  let oks (e, t) = Ok ((e, span), t) in
  match cst with
  | CValue (VUnit)   -> oks (TValue (VUnit),   constr "Unit")
  | CValue (VBool b) -> oks (TValue (VBool b), constr "Bool")
  | CValue (VInt i)  -> oks (TValue (VInt i),  constr "Int")
  | CValue (VStr s)  -> oks (TValue (VStr s),  constr "String")
  | CSym s -> (match infer_get_scheme inf s with
    | Some sch ->
      let t = sch_instantiate sch inf.ctx in
      oks (TSym s, t)
    | None -> err_ret (Printf.sprintf "Unbound symbol '%s'" s) span)
  | CApp (f, x) ->
    let* (f, f_t) = infer_cst inf f in
    let* (x, x_t) = infer_cst inf x in
    let ret_t = infer_fresh inf in

    let* _ = (match ctx_unify inf.ctx (arrow x_t [ret_t]) f_t with
      | Ok _    -> Ok ()
      | Error e -> err_ret (unify_error_to_string e) span)
    in

    oks (TApp (f, x), ret_t)
  | CBin (l, op, r) ->
    let* (l, l_t) = infer_cst inf l in
    let* (r, r_t) = infer_cst inf r in

    let (expected_t, ret_t) = match op with
      | Add | Sub | Mul | Div | Mod
      -> (constr "Int", constr "Int")
      | Eq | Neq | Lt | Gt | Le | Ge
      -> (infer_fresh inf, constr "Bool")
      | And | Or
      -> (constr "Bool", constr "Bool")
    in

    let* _ = unify inf.ctx l_t expected_t (snd l) in
    let expected_t = tp_apply inf.ctx expected_t in
    let* _ = unify inf.ctx r_t expected_t (snd r) in

    oks (TBin (l, op, r), ret_t)
  | CLambda (arg, body) ->
    let param_t = infer_fresh inf in
    let param_id = match param_t with | Var id -> id | _ -> failwith "unreachable" in

    infer_set_scheme inf (fst arg) (tp_generalize param_t [param_id]);
    let* (body, body_t) = infer_cst inf body in
    infer_unset_scheme inf (fst arg);

    let t = arrow param_t [body_t] in
    oks (TLambda (arg, body), t)
  | CCase { value; br; default_br } ->
    let* (value, value_t) = infer_cst inf value in

    let ret_t = infer_fresh inf in
    let* br = map_early_return (fun (v, b) ->
      let* v_t = match fst v with
        | VUnit   -> Ok (constr "Unit")
        | VBool _ -> Ok (constr "Bool")
        | VInt _  -> Ok (constr "Int")
        | VStr _  -> Ok (constr "String")
      in
      let* (b, b_t) = infer_cst inf b in

      (* Unify pat with scrutinee *)
      let* _ = unify inf.ctx value_t v_t (snd v) in
      (* Unify branch with return type *)
      let* _ = unify inf.ctx b_t ret_t (snd b) in

      Ok (v, b)
    ) br in

    (* Unify default branch *)
    let* (default_br, default_br_t) = infer_cst inf default_br in
    let* _ = unify inf.ctx default_br_t ret_t (snd default_br) in

    oks (TCase { value; br; default_br }, ret_t)
  | CLet { name; value; body } ->
    let* (value, value_t) = infer_cst inf value in

    let value_tp = tp_apply inf.ctx value_t in
    infer_set_scheme inf (fst name) (tp_generalize value_tp []);

    let* (body, body_t) = infer_cst inf body in
    infer_unset_scheme inf (fst name);

    oks (TLet { name; typ = value_tp; value; body }, body_t)
  | CThen (c1, c2) ->
    let* (c1, _) = infer_cst inf c1 in
    let* (c2, c2_t) = infer_cst inf c2 in
    oks (TThen (c1, c2), c2_t)

let infer_top inf top =
  let (top, span) = top in
  match top with
  | CTUse _ -> Ok (None)
  | CTAnno (name, tp) ->
    infer_set_scheme inf (fst name) (tp_generalize (fst tp) []);
    Ok (None)
  | CTDef (name, body) ->
    let anno_tp = match infer_get_scheme inf (fst name) with
      | Some sch -> sch_instantiate sch inf.ctx
      | None -> infer_fresh inf in

    let* (body, body_tp) = infer_cst inf body in

    let* _ = ctx_unify inf.ctx body_tp anno_tp |>
      (function
        | Ok _ -> Ok ()
        | Error e -> 
          err_ret (unify_error_to_string e) (snd body))
    in
    let tp = tp_apply inf.ctx anno_tp in

    infer_set_scheme inf (fst name) (tp_generalize tp []);

    Ok (Some (TTDef (name, tp, body), span))

let infer tops =
  let inf = infer_new () in

  (* add built-in functions *)
  let builtins =
    (* magic0: String -> 'a. ex: magic0 "flush" *)
    [ ("magic0", sch_new [-1]     (arrow (constr "String") [Var (-1)]))
    (* magic1: String -> 'a -> 'b. ex: magic1 "print" "hi" *)
    ; ("magic1", sch_new [-1; -2] (arrow (constr "String") [Var (-1); Var (-2)]))
    ]
  in
  List.iter (fun (name, sch) -> infer_set_scheme inf name sch) builtins;

  let rec aux acc err_acc = function
    | [] -> (List.rev acc, List.rev err_acc)
    | top :: rest ->
      match infer_top inf top with
      | Ok res ->
        (match res with
        | Some t -> aux (t :: acc) err_acc rest
        | None   -> aux acc err_acc rest)
      | Error e ->
        aux acc (e :: err_acc) rest
  in

  aux [] [] tops