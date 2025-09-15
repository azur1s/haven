open Common

type closure =
  { entry : string
  ; actual_fvs : cl_ast list
  } [@@deriving show]

and cl_ast =
  | CValue  of value
  | CSym    of string
  | CList   of cl_ast list
  | CAppDir of string * cl_ast list
  | CBin    of cl_ast * bin * cl_ast
  | CLambda of string list * cl_ast
  | CCase of
    { value: cl_ast
    ; br: (value * cl_ast) list
    ; default_br: cl_ast
    }
  | CLet of
    { name: string
    ; value: cl_ast
    ; body: cl_ast
    }
  | CThen    of cl_ast * cl_ast
  | CMakeCls of closure
  | CAppCls  of cl_ast * cl_ast list
  [@@deriving show]