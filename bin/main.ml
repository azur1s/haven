open Ichor.Loc
open Ichor.Lex
open Ichor.Parse
open Cmdliner

let readfile path =
  let ch = open_in_bin path in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let process path =
  let ic = open_in path in
  try
    let content = readfile path in
    match lex content ~file:path with
    | Ok xs ->
      (match parse xs ~file:path with
      | Ok tops -> List.iter (fun (t, _) -> print_endline @@ show_cst_top t) tops
      | Error (m, loc) -> print_endline @@ m ^ " @ " ^ show_span_no_file loc)
    | Error (m, loc) -> print_endline @@ m ^ " @ " ^ show_span_no_file loc;
  with e ->
    close_in_noerr ic;
    raise e

let path =
  let doc = "The input file" in
  Arg.(value & pos 0 string "" & info [] ~docv:"INPUT" ~doc)
let process_t = Term.(const process $ path)
let cmd = Cmd.v (Cmd.info "input file") process_t

let () = exit (Cmd.eval cmd)
