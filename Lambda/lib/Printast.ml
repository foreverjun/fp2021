type 'name t = 'name Ast.t =
  | Var of 'name
  | Abs of 'name * 'name t
  | App of 'name t * 'name t
[@@deriving show { with_path = false }]

let pp_named = pp Format.pp_print_string
