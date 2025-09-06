open Common
open Core

type js =
  | JsValue  of value
  | JsSym    of string
  | JsApp    of js * js list
  | JsBin    of js * bin * js
  | JsLambda of string list * js
  | JsIf     of js * js * js
  | JsThen   of js * js
  | JsDef    of string * js
  | JsIIFE   of string list * js list * js list
  [@@deriving show]

let rec string_of_js = function
  | JsValue v -> string_of_value v
  | JsSym s   -> s
  | JsApp (f, args) ->
    Printf.sprintf "%s(%s)"
      (string_of_js f)
      (String.concat ", " (List.map string_of_js args))
  | JsBin (l, b, r) ->
    Printf.sprintf "%s %s %s"
      (string_of_js l)
      (string_of_bin b)
      (string_of_js r)
  | JsLambda (args, body) ->
    Printf.sprintf "(%s) => %s"
      (String.concat ", " args)
      (string_of_js body)
  | JsIf (cond, thn, els) ->
    Printf.sprintf "(%s ? %s : %s)"
      (string_of_js cond)
      (string_of_js thn)
      (string_of_js els)
  | JsThen (l, r) ->
    Printf.sprintf "%s; %s"
      (string_of_js l)
      (string_of_js r)
  | JsDef (name, value) ->
    Printf.sprintf "var %s = %s" name (string_of_js value)
  | JsIIFE (args, body, params) ->
    let body_init = List.rev (List.tl (List.rev body)) in
    let body_last = List.hd (List.rev body) in
    Printf.sprintf "((%s) => { %s; return %s })(%s)"
      (String.concat ", " args)
      (String.concat "; " (List.map string_of_js body_init))
      (string_of_js body_last)
      (String.concat ", " (List.map string_of_js params))

let rec core_to_js c = match c with
  | LValue v -> JsValue v
  | LSym s   -> JsSym s

  | LApp (LSym "_magic1", LValue (VStr "print") :: rest) ->
    JsApp (JsSym "process.stdout.write", List.map core_to_js rest)

  | LApp (LSym "_magic1", LValue (VStr "to_string") :: rest) ->
    JsApp (JsSym "String", List.map core_to_js rest)

  | LApp (LSym "_magic1", x :: _) ->
    failwith ("_magic1: unknown function: " ^ (string_of_core x))

  | LApp (f, args) ->
    JsApp (core_to_js f, List.map core_to_js args)
  | LBin (l, b, r) ->
    JsBin (core_to_js l, b, core_to_js r)
  | LLambda (args, body) ->
    JsLambda (args, core_to_js body)
  | LCase { value; br; default_br } ->
    let rec make_if = function
      | [] -> core_to_js default_br
      | (v, arm) :: rest ->
        JsIf (JsBin (core_to_js value, Eq, JsValue v),
              core_to_js arm,
              make_if rest)
    in
    make_if br
  | LLet { name; value; body } ->
    (* JsIIFE ([name], [core_to_js body], [core_to_js value]) *)
    JsIIFE ([], [JsDef (name, core_to_js value); core_to_js body], [])
  | LThen (l, r) ->
    (* JsIIFE ([], [core_to_js r], [core_to_js l]) *)
    JsIIFE ([], [core_to_js l; core_to_js r], [])

let ctop_to_js c = match c with
  | LTDef (name, _typ, body) ->
    JsDef (name, core_to_js body)

let compile cs =
  let out = List.map ctop_to_js cs
    |> List.map string_of_js
    |> String.concat "\n"
  in
  out ^ "\nmain()\n"
