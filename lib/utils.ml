let todo ?(reason="") loc =
  failwith @@ "TODO" ^ (if reason != "" then " " ^ reason else "") ^ " " ^ loc

let unreachable loc = failwith @@ "Unreachable " ^ loc