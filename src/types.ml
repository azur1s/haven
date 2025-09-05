module IntMap = Map.Make(struct
  type t = int
  let compare = Int.compare
end)

module IntSet = Set.Make(struct
  type t = int
  let compare = Int.compare
end)

type tp =
  | Constr of string * tp list
  | Var    of int
  [@@deriving show]

let rec tp_show is_return tp = match tp with
  | Constr (name, []) -> name
  | Constr (name, args) when name = "->" || name = "→" ->
      let arr = Printf.sprintf "%s → %s"
        (List.hd args |> tp_show false)
        (List.tl args |> List.hd |> tp_show true) in
      if not is_return
        then Printf.sprintf "(%s)" arr
        else arr
  | Constr (name, args) ->
      Printf.sprintf "%s(%s)"
      name (String.concat ", " (List.map (tp_show true) args))
  | Var i -> "t" ^ (string_of_int i)
and string_of_tp tp = tp_show true tp

type scheme =
  | Mono of tp
  | Poly of int * scheme
  [@@deriving show]

type ctx =
  { mutable subst: tp IntMap.t
  ; mutable next: int
  }

let string_of_subst subst =
  IntMap.bindings subst
  |> List.map (fun (k, v) -> Printf.sprintf "t%d -> %s" k (show_tp v))
  |> String.concat ", "

type unify_error =
  | Occurs of int
  | Failure of tp * tp
  [@@deriving show]

let unify_error_to_string = function
  | Occurs v -> Printf.sprintf "Occurs check failed for variable t%d" v
  | Failure (t, u) -> Printf.sprintf "Cannot unify %s with %s" (string_of_tp t) (string_of_tp u)

(* tp *)

let constr name = Constr (name, [])
let app name args = Constr (name, args)
let rec arrow (f : tp) (xs : tp list) =
  match xs with
  | [] -> f
  | arg :: ret -> Constr ("->", [f; arrow arg ret])

let tp_get_vars tp =
  let rec aux acc = function
    | Constr (_, args) -> List.fold_left aux acc args
    | Var i -> IntSet.add i acc
  in
  aux IntSet.empty tp |> IntSet.elements

let rec tp_occurs tp v = match tp with
  | Constr (_, args) -> List.exists (fun arg -> tp_occurs arg v) args
  | Var i -> i = v

let rec tp_apply ctx tp = match tp with
  | Constr (name, args) ->
    let args = List.map (tp_apply ctx) args in
    Constr (name, args)
  | Var i ->
    match IntMap.find_opt i ctx.subst with
    | Some tp -> tp_apply ctx tp
    | None -> Var i

let tp_generalize tp bound =
  let free_vars = tp_get_vars tp
    |> List.filter (fun v -> not (List.mem v bound)) in
  List.fold_right (fun v acc -> Poly (v, acc)) free_vars (Mono tp)

let rec tp_substitute subst tp = match tp with
  | Constr (name, args) ->
      let args = List.map (tp_substitute subst) args in
      Constr (name, args)
  | Var i ->
      match IntMap.find_opt i subst with
      | Some tp' -> tp_substitute subst tp'
      | None -> Var i

(* ctx *)

let ctx_new () = { subst = IntMap.empty; next = 0 }

let ctx_new_var ctx =
  (* ctx := { !ctx with next = !ctx.next + 1 }; *)
  ctx.next <- ctx.next + 1;
  Var (ctx.next - 1)

let ctx_extend ctx v tp =
  if v >= ctx.next then ctx.next <- v + 1;
  ctx.subst <- IntMap.add v tp ctx.subst

let ctx_unify ctx t u =
  let rec unify t u =
    if t = u then Ok ()
    else match (t, u) with
    | Var v, u -> if tp_occurs u v then
        Error (Occurs v)
      else (
        ctx_extend ctx v u;
        Ok ()
      )
    | t, Var v -> if tp_occurs t v then
        Error (Occurs v)
      else (
        ctx_extend ctx v t;
        Ok ()
      )
    | Constr (n1, a1), Constr (n2, a2) ->
      if n1 <> n2 then
        Error (Failure (t, u))
      else
        List.fold_left2 (fun acc t u ->
          match acc with
          | Error _ as e -> e
          | Ok () ->
            let t = tp_apply ctx t in
            let u = tp_apply ctx u in
            unify t u
        ) (Ok ()) a1 a2
  in

  let rollback = ctx.subst in

  let t = tp_apply ctx t in
  let u = tp_apply ctx u in
  let res = unify t u in

  if Result.is_error res then ctx.subst <- rollback;
  res

(* scheme *)

let sch_new vars tp =
  List.fold_right (fun v acc -> Poly (v, acc)) vars (Mono tp)

let sch_instantiate sch ctx =
  let rec aux sch =
    match sch with
    | Mono tp -> tp_substitute ctx.subst tp
    | Poly (v, rest) ->
      let tp = ctx_new_var ctx in
      ctx_extend ctx v tp;
      aux rest
  in
  aux sch
