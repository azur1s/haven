open Common

let process file =
  let start_time = Sys.time () in

  let ic = Stdio.In_channel.create file in
  let input = (Stdio.In_channel.input_all ic) ^ "\n" in
  Stdio.In_channel.close ic;

  (* let output_file = (Filename.remove_extension file) ^ ".js" in *)
  let output_file = "out.scm" in

  let parse file str =
      let* tokens = Lex.lex ~file str
        |> fun x -> match x with
          | Ok xs -> Ok xs
          | Error e -> Error [e]
      in

      let* ctop = Parse.parse ~file tokens
        |> fun x -> match x with
          | Ok xs -> Ok xs
          | Error e -> Error [e]
      in
      Ok ctop
  in

  let* ctop = parse file input in

  let uses = List.filter_map (fun (ctop, _) -> match ctop with
    | Parse.CTUse name -> Some name
    | _ -> None) ctop in

  let* uses_ctop =
    uses
    |> List.flatten
    |> (fun xs -> List.map (fun (_name, path) ->
      let use_file = (Filename.dirname file) ^ "/" ^ (fst path) ^ ".hvn" in
      if Sys.file_exists use_file then (
        let ic = Stdio.In_channel.create use_file in
        let input = (Stdio.In_channel.input_all ic) ^ "\n" in
        Stdio.In_channel.close ic;
        (* let fp = Unix.realpath use_file in *)
        let fp = use_file in
        parse fp input
      ) else
        Error [{ msg = "Could not find module: " ^ use_file
               ; loc = snd path
               ; hint = None
               }]
    ) xs)
    |> combine_results in

  let ctop = List.flatten uses_ctop @ ctop in

  (* List.iter (fun t -> Printf.printf "%s\n" (Parse.show_ctop (fst t))) ctop; *)

  let (top, infer_err) = Infer.infer ctop in

  let* _ = if List.length infer_err > 0 then
    Error infer_err
  else (
    let out = Core.transform top
      (* |> List.map (fun t -> print_endline (Core.string_of_ctop t); t) *)
      |> Scheme.compile
    in

    let prelude = In_channel.with_open_text "lib/scheme/prelude.scm" In_channel.input_all in
    let out = prelude ^ out in

    let oc = Stdio.Out_channel.create output_file in
    Stdio.Out_channel.output_string oc out;
    Stdio.Out_channel.close oc;
    Ok ()) in

  let end_time = Sys.time () in

  Printf.printf "Compilation successful in %.2f seconds.\n" (end_time -. start_time);
  Printf.printf "Written to %s\n" output_file;

  Ok ()

let report err =
  Printf.eprintf "Error: %s at %s line %s characters %s-%s\n"
    err.msg
    err.loc.file
    (string_of_int (fst err.loc.pos_start))
    (string_of_int (snd err.loc.pos_start))
    (string_of_int (snd err.loc.pos_end));
  (match err.hint with
    | Some h -> Printf.eprintf "Hint: %s\n" h
    | None -> ())

let () =
  Printexc.record_backtrace true;
  let args = Array.to_list Sys.argv |> List.tl in
  match args with
  | [] -> Printf.eprintf "Usage: %s <file>\n" Sys.argv.(0)
  | file :: _ ->
    if Sys.file_exists file then
      process file
      |> Result.iter_error (List.iter report)
    else
      Printf.eprintf "File not found: %s\n" file
