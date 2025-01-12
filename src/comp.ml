open Common
open Utils
open Norm
open Sexplib0

type js_expr =
  | JSLit of lit
  | JSList of js_expr list
  | JSTuple of js_expr list
  | JSObject of (string * js_expr) list
  | JSAccess of js_expr * string
  | JSBin of js_expr * bin * js_expr
  | JSApp of js_expr * js_expr list
  | JSArrow of string list * js_expr
  | JSTernary of js_expr * js_expr * js_expr
  | JSThen of js_expr * js_expr
  | JSDef of string * js_expr
  | JSBlock of js_expr
  | JSReturn of js_expr

let string_of_js_bin = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "==="
  | Neq -> "!=="
  | Lt -> "<"
  | Lte -> "<="
  | Gt -> ">"
  | Gte -> ">="
  | And -> "&&"
  | Or -> "||"
  | Cons -> todo __LOC__ ~reason:"cons binop"

let rec string_of_js_expr = function
  | JSLit (LStr s) ->
    (* Somehow \r\n will be interpreted as \n\n for Windows *)
    let s = Str.global_replace (Str.regexp "\r") "" s in
    Printf.sprintf "`%s`" s
  | JSLit l -> string_of_lit l
  | JSList l -> Printf.sprintf "[%s]" (String.concat ", " (List.map string_of_js_expr l))
  | JSTuple l -> Printf.sprintf "[%s]" (String.concat ", " (List.map string_of_js_expr l))
  | JSObject l -> Printf.sprintf "{%s}"
    (String.concat ", " (List.map (fun (k, v) -> Printf.sprintf "%s: %s" k (string_of_js_expr v)) l))
  | JSAccess (a, b) -> Printf.sprintf "%s.%s" (string_of_js_expr a) b
  | JSBin (a, Cons, b) ->
    Printf.sprintf "[%s].concat(%s)" (string_of_js_expr a) (string_of_js_expr b)
  | JSBin (a, op, b) ->
    Printf.sprintf "(%s %s %s)" (string_of_js_expr a) (string_of_js_bin op) (string_of_js_expr b)
  | JSApp (f, args) ->
    Printf.sprintf "%s(%s)" (string_of_js_expr f) (String.concat ", " (List.map string_of_js_expr args))
  | JSArrow (args, body) ->
    Printf.sprintf "((%s) => %s)" (String.concat ", " args) (string_of_js_expr body)
  | JSTernary (cond, then_, else_) ->
    Printf.sprintf "(%s ? %s : %s)" (string_of_js_expr cond) (string_of_js_expr then_) (string_of_js_expr else_)
  | JSThen (a, b) ->
    Printf.sprintf "%s;\n%s" (string_of_js_expr a) (string_of_js_expr b)
  | JSDef (name, expr) ->
    Printf.sprintf "const %s = %s" name (string_of_js_expr expr)
  | JSBlock expr ->
    Printf.sprintf "{\n%s\n}" (string_of_js_expr expr)
  | JSReturn expr ->
    Printf.sprintf "return %s" (string_of_js_expr expr)

module M = Map.Make(String)

type ctx =
  { mutable id: int
  ; mutable main_is_defined: bool
  }

let reset_id ctx =
  ctx.id <- 0

let next_id ctx =
  ctx.id <- ctx.id + 1;
  ctx.id

let rec put_return_in_then_chain = function
  | JSThen (a, b) -> JSThen (a, put_return_in_then_chain b)
  | x -> JSReturn x

let rec comp_term ctx term =
  match term with
  | KLit l -> JSLit l
  | KList l -> JSList (List.map (comp_term ctx) l)
  | KTuple l -> JSTuple (List.map (comp_term ctx) l)
  | KRecord l -> JSObject (List.map (fun (k, v) -> (k, comp_term ctx v)) l)
  | KAccess (r, k) -> JSAccess (comp_term ctx r, k)
  | KBin (KList l, Eq, x) | KBin (x, Eq, KList l) ->
    JSApp (JSLit (LSym "list_eq"), [JSList (List.map (comp_term ctx) l); comp_term ctx x])
  | KBin (a, op, b) -> JSBin (comp_term ctx a, op, comp_term ctx b)
  | KApp (KLit (LSym "__js__"), args) ->
    (match args with
    | KLit (LStr f) :: args -> JSApp (JSLit (LSym f), List.map (comp_term ctx) args)
    | x -> List.map show_kterm x
      |> String.concat ", "
      |> (^) __LOC__
      |> (^) "Invalid external call: "
      |> failwith)
  | KApp (KLit (LSym "__js_method__"), args) ->
    (match args with
    | [obj; KLit (LStr field); KList args] ->
      JSApp (JSAccess (comp_term ctx obj, field), List.map (comp_term ctx) args)
    | x -> List.map show_kterm x
      |> String.concat ", "
      |> (^) __LOC__
      |> (^) "Invalid external method call: "
      |> failwith)
  | KApp (KLit (LSym "__js_field__"), args) ->
    (match args with
    | obj :: KLit (LStr field) :: [] -> JSAccess (comp_term ctx obj, field)
    | x -> List.map show_kterm x
      |> String.concat ", "
      |> (^) __LOC__
      |> (^) "Invalid external field access: "
      |> failwith)
  | KApp (f, args) -> JSApp (comp_term ctx f, List.map (comp_term ctx) args)
  | KLambda (args, body) -> JSArrow (args, comp_term ctx body)
  | KIf { cond; t; f } ->
    let cond = comp_term ctx cond in
    let t = comp_term ctx t in
    let f = comp_term ctx f in
    JSTernary (cond,
      JSApp (JSArrow ([], JSBlock (put_return_in_then_chain t)), []),
      JSApp (JSArrow ([], JSBlock (put_return_in_then_chain f)), []))
  | KDef { name; body; in_ } ->
    let body = comp_term ctx body in
    let in_ = comp_term ctx in_ in
    JSThen (JSDef (name, body), in_)
  | KFun _ ->
    Printf.printf "KFun: %s\n" (sexp_of_kterm term |> Sexp.to_string);
    failwith "KFun should've been converted to let lambda"
  | e -> todo __LOC__ ~reason:(show_kterm e)

let comp_top ctx top =
  let process k =
    comp_term ctx k
    |> put_return_in_then_chain in
  match top with
  | KTDef (name, body, _) ->
    reset_id ctx;
    JSDef (name, JSApp (JSArrow ([], JSBlock (process body)), []))
  | KTFun { name; args; body; _ } ->
    reset_id ctx;
    if name = "main" then
      ctx.main_is_defined <- true;
    JSDef (name, JSArrow (args, JSBlock (process body)))

let comp terms =
  let ctx =
    { id = 0
    ; main_is_defined = false
    }
  in
  let comped = List.map (comp_top ctx) terms in
  if ctx.main_is_defined then
    comped @ [JSApp (JSLit (LSym "main"), [])]
  else
    failwith "main function is not defined"
