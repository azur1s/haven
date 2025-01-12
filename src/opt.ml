open Common
open Norm

let sep_from pred lst =
  let rec aux acc = function
    | [] -> None
    | x :: xs -> if pred x then Some (x, List.rev_append acc xs) else aux (x :: acc) xs
  in
  aux [] lst

type elim_context =
  { mutable alive : ktop list
  ; mutable dead : ktop list
  }

let elim_create dead =
  { alive = []
  ; dead
  }

let name_of_top = function
  | KTDef (name, _, _) -> name
  | KTFun { name; _ } -> name

let id_of_top = function
  | KTDef (_, _, id) -> id
  | KTFun { id; _ } -> id

let elim_resurrect (ctx: elim_context ref) top =
  ctx := { !ctx with alive = !ctx.alive @ [top] }

let elim_mark (ctx: elim_context ref) name =
  (* TODO v handle shadowing (see tests/shadow.ich) *)
  (* if List.exists (fun x -> name_of_top x = name) !ctx.alive then () else *)
  match sep_from (fun n -> name_of_top n = name) !ctx.dead with
  | None ->
    if List.exists (fun x -> name_of_top x = name) !ctx.alive
      then ()
        (* Check if body is same by id field *)
        (* let old_id = List.find (fun x -> id_of_top x = name) !ctx.alive in
        let new_id =  *)
      else print_endline @@ "unreachable: " ^ name
  | Some (new_alive, new_dead) ->
    ctx := { alive = new_alive :: !ctx.alive; dead = new_dead }

let elim_walk (ctx: elim_context ref) tops =
  (* Find function called "main" *)
  let main = List.find (fun x -> name_of_top x = "main") tops in

  let rec walk_term term = match term with
    | KLit (LSym s) ->
      (* print_endline @@ "alive " ^ List.fold_left (fun acc x -> acc ^ x ^ ", ") "" (List.map name_of_top !ctx.alive); *)
      (* print_endline @@ "dead  " ^ List.fold_left (fun acc x -> acc ^ x ^ ", ") "" (List.map name_of_top !ctx.dead); *)
      (* print_endline s; *)
      elim_mark ctx s; term
    | KApp (f, args) ->
      let f = walk_term f in
      let args = List.map walk_term args in
      KApp (f, args)
    | KDef { name; body; in_ } ->
      let body = walk_term body in
      let in_ = walk_term in_ in
      KDef { name; body; in_ }
    | _ -> term
  in

  let walk top = match top with
    | KTFun { name; args; recr; body; id } ->
      let body = walk_term body in
      elim_resurrect ctx (KTFun { name; args; recr; body; id });
      ()
    | _ ->
      print_endline "unreachable: how"
  in
  walk main

let optim level (tops: ktop list) =
  match level with
  | 0 -> tops
  | _ ->
    let ctx = ref (elim_create tops) in
    elim_walk ctx tops;
    !ctx.alive