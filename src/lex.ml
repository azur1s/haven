open Common

type token =
  | TkUnit
  | TkBool of bool
  | TkInt of int
  | TkStr of string
  | TkSym of string
  | TkBin of bin
  | TkCase | TkElse | TkBar
  | TkVal | TkLet | TkIn | TkSemi
  | TkAssign | TkColon | TkDot
  | TkLambda | TkArrow
  | TkComma
  | TkUse
  | TkL of char
  | TkR of char
  [@@deriving show]

let string_of_token = function
  | TkUnit -> "()"
  | TkBool b -> string_of_bool b
  | TkInt i -> string_of_int i
  | TkStr s -> "\"" ^ s ^ "\""
  | TkSym s -> s
  | TkBin b -> string_of_bin b
  | TkCase -> "case"
  | TkElse -> "else"
  | TkBar -> "|"
  | TkVal -> "val"
  | TkLet -> "let"
  | TkIn -> "in"
  | TkSemi -> ";"
  | TkAssign -> "="
  | TkColon -> ":"
  | TkLambda -> "\\"
  | TkArrow -> "->"
  | TkDot -> "."
  | TkComma -> ","
  | TkUse -> "use"
  | TkL c -> String.make 1 c
  | TkR c -> String.make 1 c

type lexer =
  { file: string
  ; input: string
  ; mutable index: int
  ; mutable pos: int * int (* for reporting *)
  }

let make_span l start =
  { file = l.file
  ; pos_start = start
  ; pos_end = l.pos
  }

let peek l =
  if l.index < String.length l.input then
    Some l.input.[l.index]
  else
    None

let advance l =
  if l.index < String.length l.input then (
    let char = l.input.[l.index] in
    let (line, col) = l.pos in
    if char = '\n' then
      l.pos <- (line + 1, 1)
    else
      l.pos <- (line, col + 1);
    l.index <- l.index + 1;
    Some char
  ) else
    None

let is_padding = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false

let is_atom_char = function
  | 'a'..'z' | 'A'..'Z' | '_' | '\'' -> true
  | _ -> false

let rec tokenize_acc l acc =
  match peek l with
  | None -> Ok (List.rev acc)
  | Some c ->
    let start = l.pos in

    let when_peek_is f =
      match peek { l with index = l.index + 1 } with
      | Some c -> f c
      | None -> false
    in

    let single char =
      let _ = advance l in
      let span = make_span l start in
      tokenize_acc l @@ (char, span) :: acc
    in

    match c with
    | c when is_padding c ->
      let _ = advance l in
      tokenize_acc l acc
    | c when c = '(' && when_peek_is ((=) '*') ->
      let _ = advance l in
      let _ = advance l in
      let rec skip_comment () =
        match peek l with
        | Some '*' when when_peek_is ((=) ')') ->
          let _ = advance l in
          let _ = advance l in
          ()
        | Some _ ->
          let _ = advance l in
          skip_comment ()
        | None -> ()
      in
      skip_comment ();
      tokenize_acc l acc
    | c when c = ';' && when_peek_is ((=) ';') ->
      let _ = advance l in
      let _ = advance l in
      let rec skip_line () =
        match peek l with
        | Some '\n' ->
          let _ = advance l in
          ()
        | Some _ ->
          let _ = advance l in
          skip_line ()
        | None -> ()
      in
      skip_line ();
      tokenize_acc l acc
    | c when c = '(' && when_peek_is ((=) ')') ->
      let _ = advance l in
      let _ = advance l in
      let span = make_span l start in
      tokenize_acc l @@ (TkUnit, span) :: acc
    | c when c = '-' && when_peek_is ((=) '>') ->
      let _ = advance l in
      let _ = advance l in
      let span = make_span l start in
      tokenize_acc l @@ (TkArrow, span) :: acc

    | c when c = '=' && when_peek_is ((=) '=') ->
      advance l |> ignore;
      advance l |> ignore;
      let span = make_span l start in
      tokenize_acc l @@ (TkBin Eq, span) :: acc
    | c when c = '!' && when_peek_is ((=) '=') ->
      advance l |> ignore;
      advance l |> ignore;
      let span = make_span l start in
      tokenize_acc l @@ (TkBin Neq, span) :: acc
    | c when c = '<' && when_peek_is ((=) '=') ->
      advance l |> ignore;
      advance l |> ignore;
      let span = make_span l start in
      tokenize_acc l @@ (TkBin Le, span) :: acc
    | c when c = '>' && when_peek_is ((=) '=') ->
      advance l |> ignore;
      advance l |> ignore;
      let span = make_span l start in
      tokenize_acc l @@ (TkBin Ge, span) :: acc
    | c when c = '&' && when_peek_is ((=) '&') ->
      advance l |> ignore;
      advance l |> ignore;
      let span = make_span l start in
      tokenize_acc l @@ (TkBin And, span) :: acc
    | c when c = '|' && when_peek_is ((=) '|') ->
      advance l |> ignore;
      advance l |> ignore;
      let span = make_span l start in
      tokenize_acc l @@ (TkBin Or, span) :: acc

    | '+' -> single (TkBin Add)
    | '-' -> single (TkBin Sub)
    | '*' -> single (TkBin Mul)
    | '/' -> single (TkBin Div)
    | '%' -> single (TkBin Mod)
    | '<' -> single (TkBin Lt)
    | '>' -> single (TkBin Gt)

    | '|' -> single TkBar
    | ';' -> single TkSemi
    | '=' -> single TkAssign
    | ':' -> single TkColon
    | '\\' -> single TkLambda
    | '.' -> single TkDot
    | ',' -> single TkComma
    | '(' -> single (TkL c)
    | ')' -> single (TkR c)
    | '[' -> single (TkL c)
    | ']' -> single (TkR c)
    | '{' -> single (TkL c)
    | '}' -> single (TkR c)
    | '"' ->
      let _ = advance l in
      let rec read_str acc =
        match peek l with
        | Some '"' ->
          let _ = advance l in
          acc
        | Some '\\' ->
          let _ = advance l in
          (match peek l with
          | Some c ->
            let _ = advance l in
            read_str (acc ^ "\\" ^ String.make 1 c)
          | None -> acc)
        | Some c ->
          let _ = advance l in
          read_str (acc ^ String.make 1 c)
        | None -> acc
      in
      let str = read_str "" in
      let span = make_span l start in
      tokenize_acc l @@ (TkStr str, span) :: acc
    | c when is_digit c ->
      let rec read_int acc =
        match peek l with
        | Some c when is_digit c ->
          let _ = advance l in
          read_int (acc * 10 + (int_of_char c - int_of_char '0'))
        | _ -> TkInt acc
      in
      let _ = advance l in
      let int_val = read_int (int_of_char c - int_of_char '0') in
      let span = make_span l start in
      tokenize_acc l @@ (int_val, span) :: acc
    | c when is_atom_char c ->
      let rec read_atom acc =
        match peek l with
        | Some c when is_atom_char c || is_digit c ->
          let _ = advance l in
          read_atom (acc ^ String.make 1 c)
        | _ ->
          match acc with
          | "true"  -> TkBool true
          | "false" -> TkBool false
          | "case"  -> TkCase
          | "else"  -> TkElse
          | "val"   -> TkVal
          | "let"   -> TkLet
          | "in"    -> TkIn
          | "use"   -> TkUse
          | _ -> TkSym acc
      in
      let _ = advance l in
      let atom = read_atom (String.make 1 c) in
      let span = make_span l start in
      tokenize_acc l @@ (atom, span) :: acc
    | _ ->
      let span = make_span l start in
      err_ret ("Unexpected character: " ^ String.make 1 c) span

let tokenize l =
  tokenize_acc l []

let lex ?(file="<anonymous>") s =
  tokenize
    { input = s
    ; file = file
    ; index = 0
    ; pos = (1, 1)
    }