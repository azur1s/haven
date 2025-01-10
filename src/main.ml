open Common
open Lex
open Parse
open Infer
open Norm
open Comp
open Report
open Cmdliner
open Unix

let readfile path =
  let ch = open_in_bin path in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let handle_relative base_path import_path =
  (* Relative path (to the file) *)
  if String.starts_with ~prefix:"./" import_path then
    let dir = Filename.dirname base_path in
    let stripped = String.sub import_path 2
      (String.length import_path - 2) in
    let path = Filename.concat dir stripped in
    "./" ^ path ^ ".ich"
  else
  (* Standard library *)
    let importing = Filename.basename import_path in
    let core_path = match Sys.getenv_opt "ICHOR_LIB" with
      | Some s -> s ^ "/core/"
      | None -> "./core/"
    in core_path ^ importing ^ ".ich"

let rec process_lib path =
  let content = readfile path in
  match lex content ~file:path with
  | Ok xs ->
    (match parse xs ~file:path with
    | Ok (tops, uses) ->
      let uses = uses
        |> List.map (handle_relative path)
        |> List.map (process_lib) in

      let csts, errs = List.fold_left (fun acc x ->
        match x with
        | Ok    s -> ((fst acc) @ s, snd acc)
        | Error e -> (fst acc, e @ (snd acc))) ([], []) uses in

      (match errs with
      | [] -> Ok (csts @ tops)
      | errs -> Error errs)
    | Error e -> Error [e])
  | Error (m, loc) -> Error [err m loc]

let process path =
  let ic = open_in path in
  try
    let content = readfile path in
    match lex content ~file:path with
    | Ok xs ->
      (match parse xs ~file:path with
      | Ok (tops, uses) ->

        let uses = uses
          |> List.map (handle_relative path)
          |> List.map (process_lib) in

        let libs, errs = List.fold_left (fun acc x ->
          match x with
          | Ok    s -> ((fst acc) @ s, snd acc)
          | Error e -> (fst acc, e @ (snd acc))) ([], []) uses in

        (match errs with
        | [] ->
          let tops = libs @ tops in
          let (terms, infer_errs) = infer tops in
          if infer_errs = [] then (
            (* The rest of the errors after here should be compiler errors, I hope *)
            norm terms
            |> comp
            |> List.map string_of_js_expr
            |> String.concat ";\n"
            |> Result.ok)
          else
            infer_errs
            |> List.map (fun (m, loc) -> err m loc)
            |> Result.error
        | errs -> Error errs)
      | Error e -> Error [e])
    | Error (m, loc) -> Error [err m loc]
  with e ->
    close_in ic;
    raise e

let output_or_default = function
  | Some s -> s
  | None -> "out.js"

let compile path output =
  let output = output_or_default output in
  match process path with
  | Ok s -> (
    let oc = open_out output in
    Printf.fprintf oc "%s" s;
    close_out oc)
  | Error errs ->
    let ic = open_in path in
    try
      let content = readfile path in
      List.iter (report path content) errs;
      exit 1
    with e ->
      close_in ic;
      raise e

let run path output args =
  compile path output;
  let should_clean = output = None in
  let output = output_or_default output in

  let clean () = Unix.unlink output in

  let args =
    if args = [||] then
      "\"\""
    else
      String.concat " " (Array.to_list args) in
  let command = "node " ^ output ^ " " ^ args in
  match Unix.system command with
  | WEXITED 0 -> if should_clean then clean ()
  | WEXITED n ->
    print_endline "Runtime error of JavaScript execution";
    print_endline @@ "Ran: " ^ command;
    exit n
  | _ ->
    print_endline "Runtime error of JavaScript execution";
    exit 1

let path =
  let doc = "The input file" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"INPUT" ~doc)

let map_strip maybe_output =
  match maybe_output with
  | Some s -> Some (if Filename.check_suffix s ".js" then s else s ^ ".js")
  | None -> None

let output =
  let doc = "The output path without prefix (default: out)" in
  Arg.(value & opt (some string) None & info ["o"; "output"] ~docv:"OUTPUT" ~doc)
  |> Term.app (Term.const map_strip)

let process_t =
  Term.(const compile $ path $ output)
let compile_cmd =
  let doc = "Compile" in
  let info = Cmd.info "compile" ~doc in
  Cmd.v info process_t

let process_and_run_t =
  let args =
    Arg.(value & pos_right 0 string [] & info [] ~docv:"ARGS" ~doc:"Arguments to the program")
  in
  Term.(const run $ path $ output $ (const Array.of_list $ args))
let run_cmd =
  let doc = "Compile and run" in
  let info = Cmd.info "run" ~doc in
  Cmd.v info process_and_run_t

let cmds = Cmd.group (Cmd.info "") [compile_cmd; run_cmd]

let () = exit (Cmd.eval cmds)
