open Common
open Utils
open Norm

type js_expr =
  | JSLit of lit

let string_of_js_expr = function
  | JSLit l -> string_of_lit l

module M = Map.Make(String)

type ctx =
  { mutable id: int
  }

let reset_id ctx =
  ctx.id <- 0

let next_id ctx =
  ctx.id <- ctx.id + 1;
  ctx.id

let rec comp_term ctx term =
  match term with
  | KLit l -> JSLit l
  | e -> todo __LOC__ ~reason:(show_kterm e)

let comp_top ctx top =
  match top with
  | t -> todo __LOC__ ~reason:(show_ktop t)
  (* | KTDef (name, body) ->
    reset_id ctx;
    ctx.top_vars <- name :: ctx.top_vars;
    EDef (name, comp_term ctx body)
  | KTFun { name; args; body; _ } ->
    reset_id ctx;
    ctx.top_funs <- name :: ctx.top_funs;
    EFun { name; args; expr = comp_term ctx body } *)

let comp terms =
  let ctx =
    { id = 0
    }
  in
  let comped = List.map (comp_top ctx) terms in
  comped
