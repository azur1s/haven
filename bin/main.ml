open Ichor.Loc
open Ichor.Lex
open Ichor.Parse
open Ichor.Infer
open Ichor.Norm
open Ichor.Comp
open Cmdliner
open Unix

let readfile path =
  let ch = open_in_bin path in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let process path output =
  let ic = open_in path in
  try
    let content = readfile path in
    match lex content ~file:path with
    | Ok xs ->
      (match parse xs ~file:path with
      | Ok tops ->
        let (terms, infer_errs) = infer tops in
        if infer_errs = [] then (
          (* The rest of the errors after here should be compiler errors, I hope *)
          let normed = norm terms in
            comp normed
            |> List.map string_of_erl_top
            |> String.concat "\n"
            |> Printf.sprintf "-module(%s).\n%s" (Filename.basename output)
            |> Result.ok)
        else
        infer_errs
        |> List.map (fun (m, loc) -> m ^ " @ " ^ show_span_no_file loc)
        |> String.concat "\n"
        |> Result.error
      | Error (m, loc) ->
        Error (m ^ " @ " ^ show_span_no_file loc))
    | Error (m, loc) ->
      Error (m ^ " @ " ^ show_span_no_file loc)
  with e ->
    close_in ic;
    raise e

let compile path output =
  match process path output with
  | Ok s -> (
    let oc = open_out (output ^ ".erl") in
    Printf.fprintf oc "%s" s;
    close_out oc)
  | Error m -> print_endline m; exit 1

let run path output args =
  compile path output;
  let erl_file = output ^ ".erl" in

  match Unix.system @@ "erlc " ^ erl_file with
  | WEXITED 0 -> (
    let args =
      if args = [||] then
        "\"\""
      else
        String.concat " " (Array.to_list args) in
    let command = "erl -noshell -s " ^ Filename.basename output ^ " main " ^ args ^ " -s init stop" in
    match Unix.system command with
    | WEXITED 0 -> ()
    | WEXITED n ->
      print_endline "Error running the Erlang program";
      print_endline @@ "Ran: " ^ command;
      exit n
    | _ -> print_endline "Error running the Erlang program")
  | WEXITED n ->
    print_endline "Error compiling the Erlang program";
    exit n
  | _ -> print_endline "Error running the Erlang compiler"

let path =
  let doc = "The input file" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"INPUT" ~doc)

let output =
  let doc = "The output file without prefix (default: out)" in
  Arg.(value & opt string "out" & info ["o"; "output"] ~docv:"OUTPUT" ~doc)

let process_t = Term.(const compile $ path $ output)
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
