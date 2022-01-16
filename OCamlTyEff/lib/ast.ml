open Format

type exc =
  | Exc1
  | Exc2
[@@deriving eq, ord, show { with_path = false }]

let exc1 = Exc1
let exc2 = Exc2

type const =
  | CInt of int
  | CString of string
  | CBool of bool
  | CNil
[@@deriving eq]

let c_int n = CInt n
let c_string s = CString s
let c_bool b = CBool b
let c_nil = CNil

let pp_const fmt = function
  | CInt d -> fprintf fmt "%d" d
  | CString s -> fprintf fmt "%S" s
  | CBool b -> fprintf fmt "%b" b
  | CNil -> fprintf fmt "[]"
;;

type ptrn =
  | PVal of string
  | PConst of const
  | PTuple of ptrn list
  | PCons of ptrn list * ptrn
[@@deriving eq, show { with_path = false }]

let p_val name = PVal name
let p_const const = PConst const
let p_tuple ptrns = PTuple ptrns
let p_unit = p_tuple []
let p_cons ptrns ptrn = PCons (ptrns, ptrn)

let rec pp_ptrn fmt = function
  | PVal s -> fprintf fmt "%s" s
  | PConst c -> pp_const fmt c
  | PTuple l ->
    fprintf fmt "(%a)" (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_ptrn) l
  | PCons (ptrns, ptrn) ->
    pp_print_list
      ~pp_sep:(fun _ () -> ())
      (fun fmt p -> fprintf fmt "%a :: " pp_ptrn p)
      fmt
      ptrns;
    pp_ptrn fmt ptrn
;;

type decl =
  { is_rec : bool
  ; name : string
  ; expr : expr
  }
[@@deriving eq]

and expr =
  | EConst of const
  | EVal of string
  | EApp of expr * expr
  | ETuple of expr list
  | ELet of decl * expr
  | EMatch of expr * (ptrn * expr) list
  | EFun of string * expr
  | ETry of expr * (exc * expr) list
[@@deriving eq]

let e_const const = EConst const
let e_val name = EVal name
let e_app fn arg = EApp (fn, arg)
let e_tuple exprs = ETuple exprs
let e_unit = e_tuple []
let e_let decl expr = ELet (decl, expr)
let e_match scr cases = EMatch (scr, cases)
let e_fun prm_name body = EFun (prm_name, body)
let e_try scr cases = ETry (scr, cases)

let rec pp_decl fmt decl =
  fprintf
    fmt
    "let %s%s = %a"
    (if decl.is_rec then "rec " else "")
    decl.name
    pp_expr
    decl.expr

and pp_expr fmt = function
  | EConst c -> pp_const fmt c
  | EVal s -> fprintf fmt "%s" s
  | EApp (fn, arg) ->
    (match fn with
    | EVal op ->
      (match op with
      | "+"
      | "-"
      | "*"
      | "/"
      | "="
      | "!="
      | "<"
      | "<="
      | ">"
      | ">="
      | "&&"
      | "||"
      | ":="
      | "::" -> fprintf fmt "%a %s" pp_expr arg op
      | "~-" | "~!" -> fprintf fmt "(%s%a)" op pp_expr arg
      | _ -> fprintf fmt "(%s %a)" op pp_expr arg)
    | _ -> fprintf fmt "(%a %a)" pp_expr fn pp_expr arg)
  | ETuple l ->
    fprintf fmt "(%a)" (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_expr) l
  | ELet (decl, expr) -> fprintf fmt "%a in\n%a" pp_decl decl pp_expr expr
  | EMatch (scr, ptrns) ->
    fprintf
      fmt
      "match %a with\n%a"
      pp_expr
      scr
      (pp_print_list (fun fmt (p, e) -> fprintf fmt "| %a -> %a" pp_ptrn p pp_expr e))
      ptrns
  | EFun (prm, expr) -> fprintf fmt "fun %s -> %a" prm pp_expr expr
  | ETry (scr, excs) ->
    fprintf
      fmt
      "try %a with\n%a"
      pp_expr
      scr
      (pp_print_list (fun fmt (exc, expr) ->
           fprintf fmt "| %a -> %a" pp_exc exc pp_expr expr))
      excs
;;

type program = decl list [@@deriving eq]

let pp_program fmt program =
  pp_print_list (fun fmt decl -> fprintf fmt "%a\n;;\n" pp_decl decl) fmt program
;;
