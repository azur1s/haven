open Common
open Core

type schm_expr =
  | SValue of value
  | SSym   of string
  | SCons  of schm_expr list
  | SCommented of string * schm_expr
  [@@deriving show]

let rec string_of_schm_expr = function
  | SValue v -> string_of_value v
  | SSym s   -> s
  | SCons [] -> "()"
  | SCons l  -> "(" ^ (String.concat " " (List.map string_of_schm_expr l)) ^ ")"
  | SCommented (s, e) ->
    Printf.sprintf ";; %s\n%s" s (string_of_schm_expr e)

let rec core_to_schm c = match c with
  | LValue (VUnit)   -> [ SCons [ SSym "quote"; SCons [] ] ]
  | LValue (VBool b) -> [ SSym (if b then "#t" else "#f") ]
  | LValue v         -> [ SValue v ]
  | LSym s   -> [ SSym s ]
  | LApp (LSym c, LValue (VStr f) :: args)
  when List.mem c ["_call0"; "_call1"; "_call2"] ->
    let args = List.flatten (List.map core_to_schm args) in
    [ SCons (SSym f :: args) ]
  | LApp (f, args) ->
    let args = List.flatten (List.map core_to_schm args) in
    [ SCons (core_to_schm f @ args) ]
  | LList [] -> [ SCons [ SSym "quote"; SCons [] ] ]
  | LList xs ->
    let xs = List.flatten (List.map core_to_schm xs) in
    [ SCons (SSym "list" :: xs) ]
  | LBin (l, b, r) ->
    let b = match b with
      | Add -> SSym "+"
      | Sub -> SSym "-"
      | Mul -> SSym "*"
      | Div -> SSym "/"
      | Mod -> SSym "%"
      | Eq  -> SSym "equal?"
      | Neq -> SSym "_hvn_nequal?"
      | Lt  -> SSym "<"
      | Le  -> SSym "<="
      | Gt  -> SSym ">"
      | Ge  -> SSym ">="
      | And -> SSym "and"
      | Or  -> SSym "or"
    in
    (* (op l r) *)
    let l = core_to_schm l in
    let r = core_to_schm r in
    [ SCons (b :: l @ r) ]
  | LLambda (args, body) ->
    let args = SCons (List.map (fun a -> SSym a) args) in
    let body = core_to_schm body in
    [ SCons (SSym "lambda" :: args :: body) ]
  | LCase { value; br; default_br } ->
    (*
    (case value
      ((br.value) br.core...)
      (else default_br))
     *)
    let value = core_to_schm value in
    let br = br |> List.map (fun (v, a) ->
      let v = SCons [ core_to_schm (LValue v) |> List.hd ] in
      let a = core_to_schm a in
      SCons (v :: a)
    ) in
    let default_br = core_to_schm default_br in
    [ SCons (SSym "case" :: value
      @ br
      @ [ SCons (SSym "else" :: default_br) ]) ]
  | LLet { name; value; body } ->
    let value = core_to_schm value in
    let body = core_to_schm body in
    [ SCons (SSym "let" :: SCons [ SCons (SSym name :: value) ] :: body) ]
  | LThen (l, r) ->
    let l = core_to_schm l in
    let r = core_to_schm r in
    l @ r

let ctop_to_schm c =
  let (c, span) = c in
  match c with
  | LTDef (name, _typ, body) ->
    let body = core_to_schm body in
    let pos = Printf.sprintf "%s:%s:%s-%s"
      span.file
      (string_of_int (fst span.pos_start))
      (string_of_int (snd span.pos_start))
      (string_of_int (snd span.pos_end)) in
    SCommented (pos, SCons (SSym "define" :: SSym name :: body))

let compile cs =
  let out = List.map ctop_to_schm cs
    |> List.map string_of_schm_expr
    |> String.concat "\n"
  in
  out ^ "\n(main '())"