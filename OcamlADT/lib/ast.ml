type id = string [@@deriving eq, show {with_path= false}]

type bin_op =
  | Add  (**  +   *)
  | Sub  (**  -   *)
  | Mul  (**  *   *)
  | Div  (**  /   *)
  | Less  (**  <   *)
  | Leq  (**  <=  *)
  | Gre  (**  >   *)
  | Geq  (**  >=  *)
  | Eq  (** ==  *)
  | Neq  (** <>  *)
  | And  (** &&  *)
  | Or  (** ||  *)
[@@deriving eq, show {with_path= false}]

and const =
  | CBool of bool  (**  true  *)
  | CInt of int  (**1     *)
  | CString of string  (**  "qwe" *)
[@@deriving eq, show {with_path= false}]

(** | _ :: [] -> 1 *)
and case = pattern * expr

(** | Number 3      *)
and acase = id * pattern

(** | Number of int *)
and aconstr = id * tyexpr

and pattern =
  | PWild  (** _        *)
  | PConst of const  (** 1        *)
  | PVar of id  (** abc      *)
  | PCons of pattern * pattern  (** hd :: tl *)
  | PNil  (** []       *)
  | PTuple of pattern list  (** a, b     *)
  | PACase of acase
[@@deriving eq, show {with_path= false}]

and expr =
  | EConst of const  (** 1                     *)
  | EBinOp of bin_op * expr * expr  (** 1 + 1                 *)
  | EVar of id  (** abc                   *)
  | ETuple of expr list  (** 1, 2                  *)
  | ECons of expr * expr  (** hd :: tl              *)
  | EIf of expr * expr * expr  (** if true then 1 else 0 *)
  | ELet of binding * expr  (** let x e in e'         *)
  | EFun of pattern * expr  (** fun x -> x * 2        *)
  | EApp of expr * expr  (** f x                   *)
  | EMatch of expr * case list  (** match e with | _ -> 0 *)
  | ENil  (** []                    *)
  | EConstr of id * expr
[@@deriving eq, show {with_path= false}]

and binding = bool * pattern * expr [@@deriving eq, show {with_path= false}]

and decl =
  | DLet of binding  (** let x = 1                                 *)
  | DAdt of adt  (** type person = Age of int | Name of string *)
[@@deriving eq, show {with_path= false}]

and tyexpr =
  | TInt
  | TBool
  | TString
  | TList of tyexpr
  | TTuple of tyexpr list
  | TArrow of tyexpr * tyexpr  (** string -> string *)
  | TAdt of id

(** type person = Age of int | Name of string *)
and adt = id * aconstr list

and program = decl list [@@deriving eq, show {with_path= false}]
