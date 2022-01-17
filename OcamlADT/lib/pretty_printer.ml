open Ast
open Format

let rec pp_tyexpr fmt = function
  | TInt -> fprintf fmt "int"
  | TString -> fprintf fmt "string"
  | TBool -> fprintf fmt "bool"
  | TTuple ts ->
      fprintf fmt "(%a)"
        (pp_print_list
           ~pp_sep:(fun _ _ -> printf " * ")
           (fun fmt ty -> pp_tyexpr fmt ty) )
        ts
  | TList l -> fprintf fmt "%a list" pp_tyexpr l
  | TArrow _ -> fprintf fmt "->"
  | TAdt id -> fprintf fmt "%s" id
