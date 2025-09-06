open Common
open Infer
open Types

type core =
  | LValue  of value
  | LSym    of string
  | LApp    of core * core list
  | LBin    of core * bin * core
  | LLambda of string list * core
  | LCase of
    { value: core
    ; br: (value * core) list
    ; default_br: core
    }
  | LLet of
    { name: string
    (* ; typ: tp *)
    ; value: core
    ; body: core
    }
  | LThen of core * core
  [@@deriving show]

let rec string_of_core = function
  | LValue v -> string_of_value v
  | LSym s   -> s
  | LApp (f, args) ->
    Printf.sprintf "(%s %s)"
      (string_of_core f)
      (String.concat " " (List.map string_of_core args))
  | LBin (l, b, r) ->
    Printf.sprintf "(%s %s %s)"
      (string_of_core l)
      (string_of_bin b)
      (string_of_core r)
  | LLambda (args, body) ->
    Printf.sprintf "(\\%s -> %s)"
      (String.concat " " args)
      (string_of_core body)
  | LCase { value; br; default_br } ->
    let br_s = br |> List.map (fun (v, a) ->
      Printf.sprintf "| %s -> %s" (string_of_value v) (string_of_core a))
      |> String.concat "\n  "
    in
    Printf.sprintf "case %s\n  %s\n  else %s"
      (string_of_core value)
      br_s
      (string_of_core default_br)
  | LLet { name; value; body } ->
    Printf.sprintf "let %s = %s in %s"
      name
      (string_of_core value)
      (string_of_core body)
  | LThen (l, r) ->
    Printf.sprintf "%s; %s"
      (string_of_core l)
      (string_of_core r)

(* Bottom-up map *)
let core_map_child (f : core -> core) e =
  match e with
    | LValue _ | LSym _ -> e
    | LApp (e, args) -> LApp (f e, List.map f args)
    | LBin (l, b, r) -> LBin (f l, b, f r)
    | LLambda (args, body) -> LLambda (args, f body)
    | LCase { value; br; default_br } ->
      LCase { value = f value
            ; br = List.map (fun (v, a) -> (v, f a)) br
            ; default_br = f default_br }
    | LLet { name; value; body } ->
      LLet { name
           ; value = f value
           ; body = f body }
    | LThen (l, r) -> LThen (f l, f r)

(* Single-rule post-order rewrite:
   Apply on children first, then try rule on the rebuilt node. *)
let rec core_rewrite (rule : core -> core option) e =
  let e' = core_map_child (core_rewrite rule) e in
  match rule e' with
  | Some e'' when e'' <> e' -> e''
  | _ -> e'

type ctop =
  | LTDef of string * tp * core
  [@@deriving show]

let map_top f top =
  match top with
  | LTDef (name, typ, value) -> LTDef (name, typ, f value)

let string_of_ctop top =
  match top with
  | LTDef (name, typ, value) ->
    Printf.sprintf "%s: %s = %s"
      name
      (string_of_tp typ)
      (string_of_core value)

(* Lowering from typed AST to core AST *)
let rec lower ast =
  let ast = fst ast in
  match ast with
  | TValue v -> LValue v
  | TSym s   -> LSym s
  | TApp (f, a) -> LApp (lower f, [lower a])
  | TBin (l, b, r) -> LBin (lower l, b, lower r)
  | TLambda (arg, body) -> LLambda ([fst arg], (lower body))
  | TCase { value; br; default_br } ->
    LCase { value = lower value
          ; br = List.map (fun (v, a) -> (fst v, lower a)) br
          ; default_br = lower default_br }
  | TLet { name; typ = _typ; value; body } ->
    LLet { name = fst name
         ; value = lower value
         ; body = lower body }
  | TThen (l, r) -> LThen (lower l, lower r)

let lower_top top =
  let (top, _span) = top in
  match top with
  | TTDef (name, typ, value) ->
    LTDef (fst name, typ, lower value)

(* Collect left-nested application chain to n-ary application *)
let rec collect_apps e acc =
  match e with
  | LApp (f, [a])  -> collect_apps f (a :: acc)
  (* | LApp (f, args) -> collect_apps f (acc @ args) *)
  (* This should be unreachable because `lower` will retain curried apps from
     ((f a) b) to ((f [a]) [b]). *)
  | LApp _ ->
    failwith ("collect_apps: unexpected multi-arg application: " ^ show_core e)
  | _ -> (e, acc)

let uncurry_apps e =
  core_rewrite (function
    | LApp _ as e ->
      let (f, args) = collect_apps e [] in
      Some (LApp (f, args))
    | _ -> None) e

(* Collect nested lambdas to multi-arg lambda *)
let rec collect_lambdas e acc =
  match e with
  | LLambda (args, body) -> collect_lambdas body (acc @ args)
  | _ -> (e, acc)

let uncurry_lambdas e =
  core_rewrite (function
    | LLambda _ as e ->
      let (body, args) = collect_lambdas e [] in
      Some (LLambda (args, body))
    | _ -> None) e

(* ANF *)
let gensym =
  let r = ref 0 in
  fun base -> incr r; base ^ "_" ^ (string_of_int !r)

let is_trivial e =
  match e with
  | LValue _ | LSym _ -> true
  | _ -> false

let anf =
  let rec norm e k = match e with
  | LValue _ | LSym _ -> k e
  | LApp (f, args) ->
    norm_bind f (fun vf ->
    norm_binds args (fun vargs ->
      k (LApp (vf, vargs))
    ))
  | LBin (l, b, r) ->
    norm_bind l (fun vl ->
    norm_bind r (fun vr ->
      k (LBin (vl, b, vr))
    ))
  | LLambda (args, body) -> k (LLambda (args, norm body (fun b -> b)))
  | LCase { value; br; default_br } ->
    norm_bind value (fun v ->
      let br = List.map (fun (pat, arm) -> (pat, norm arm (fun a -> a))) br in
      let default_br = norm default_br (fun d -> d) in
      k (LCase { value = v; br; default_br })
    )
  | LLet { name; value; body } ->
    norm_bind value (fun v ->
      LLet { name; value = v; body = norm body k })
  | LThen (l, r) ->
    norm l (fun vl ->
      norm r (fun vr ->
        k (LThen (vl, vr))))
  and norm_bind e k =
    if is_trivial e then k e else
    let x = gensym "t" in
    norm e (fun e' -> LLet
      { name = x
      ; value = e'
      ; body = k (LSym x) })
  and norm_binds es k =
    match es with
    | [] -> k []
    | e :: rest ->
      norm_bind e (fun v ->
      norm_binds rest (fun vs ->
        k (v :: vs)))
  in
  fun e -> norm e (fun e' -> e')

let constant_folding =
  core_rewrite (function
    | LBin (LValue (VInt l), Add, LValue (VInt r)) ->
      Some (LValue (VInt (l + r)))
    | LBin (LValue (VInt l), Sub, LValue (VInt r)) ->
      Some (LValue (VInt (l - r)))
    | LBin (LValue (VInt l), Mul, LValue (VInt r)) ->
      Some (LValue (VInt (l * r)))
    | LBin (LValue (VInt l), Div, LValue (VInt r)) when r <> 0 ->
      Some (LValue (VInt (l / r)))
    | _ -> None)

(* A pass is a function that transforms a core AST *)
type 'a pass = 'a -> 'a
let ( >-> ) (p1 : 'a pass) (p2 : 'a pass) : 'a pass = fun x -> x |> p1 |> p2
let pipeline (passes : 'a pass list) : 'a pass =
  List.fold_left ( >-> ) (fun x -> x) passes

let transform ast =
  ast |> List.map (fun t -> t
    |> lower_top
    |> map_top (fun e ->
      pipeline
        [ uncurry_apps
        ; anf
        ; constant_folding
        ] e))
