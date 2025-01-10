open Sexplib0

type span =
  { file: string
  ; start: int
  ; end_: int
  } [@@deriving show]

and 'a spanned = 'a * span
  [@@deriving show]

let span_union s e =
  { file = s.file
  ; start = s.start
  ; end_ = e.end_
  }

let show_span_no_file s =
  string_of_int s.start ^ ".." ^ string_of_int s.end_

let show_span s =
  s.file ^ "@" ^ show_span_no_file s

let sexp_of_span s =
  Sexp.Atom (show_span s)
