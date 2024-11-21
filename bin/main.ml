open Ichor.Common
open Ichor.Loc
open Ichor.Lex
open Ichor.Parse
open Ichor.Infer
open Ichor.Norm
open Ichor.Comp
open Cmdliner

let readfile path =
  let ch = open_in_bin path in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let process path output =
  let ic = open_in path in
  let oc = open_out output in
  try
    let content = readfile path in
    match lex content ~file:path with
    | Ok xs ->
      (match parse xs ~file:path with
      | Ok tops ->
        let (terms, infer_errs) = infer tops in
        if infer_errs = [] then
          (output_string oc @@ Printf.sprintf "-module(%s).\n"
            (Filename.chop_extension @@ Filename.basename output);
          let normed = norm terms in
            comp normed
            |> List.map string_of_erl_top
            |> List.iter (fun s -> output_string oc s; output_string oc "\n"))
        else
          List.iter (fun (m, loc) -> print_endline @@ m ^ " @ " ^ show_span_no_file loc) infer_errs
      | Error (m, loc) -> print_endline @@ m ^ " @ " ^ show_span_no_file loc)
    | Error (m, loc) -> print_endline @@ m ^ " @ " ^ show_span_no_file loc;
  with e ->
    close_in_noerr ic;
    close_out_noerr oc;
    raise e

let path =
  let doc = "The input file" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"INPUT" ~doc)

let output =
  let doc = "The output file (default: out.erl)" in
  Arg.(value & opt string "out.erl" & info ["o"; "output"] ~docv:"OUTPUT" ~doc)

let process_t = Term.(const process $ path $ output)
let cmd = Cmd.v (Cmd.info "input file") process_t

let () = exit (Cmd.eval cmd)
