type const =
  | CInt of int
  | CString of string
  | CBool of bool
[@@deriving eq, show { with_path = false }]

type bin_operator =
  | Add 
  | Sub 
  | Mul 
  | Div 
  | Less
  | Leeq
  | Gre
  | Greq
  | Eq
  | Neq
  | And
  | Or
[@@deriving eq, show {with_path= false}]


and unary_op =
  | Minus (** - *)
  | Not (**  not  *)
[@@deriving show { with_path = false }]

type name = string [@@deriving eq, show {with_path= false}]

type expr =
  | EUnOp of unary_op * expr 
  |EConst of const
  |ECons of expr * expr
  |EBinopr of bin_operator * expr * expr
  |EVar of name
  |ETuple of expr list
  |EIf of expr * expr * expr
  |ELet of binding * expr
  |EFun of pattern * expr
  |EApp of expr * expr
  |EMatch of expr * case list
  |ENil
  |EObj of expr list
  |EMeth of pattern * expr
  |EVal of pattern * expr
  |ECallM of name * name
[@@deriving eq, show {with_path= false}]

and pattern =
  |PWild
  |PVar of name
  |PConst of const
  |PCons of pattern * pattern
  |PNil
  |PTuple of pattern list
[@@deriving eq, show {with_path= false}]

and binding = bool * pattern * expr
and case = pattern * expr

type decl = 
  |DLet of binding
[@@deriving eq, show {with_path= false}]

type progr = decl list [@@deriving eq, show {with_path= false}]
