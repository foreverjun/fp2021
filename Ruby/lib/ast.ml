type value =
  | String of string
  | Integer of int
  | Float of float
  | Boolean of bool
[@@deriving show { with_path = false }]

type identifier = Null | Identifier of string
[@@deriving show { with_path = false }]

type modifier = Local | Instance | Global | Class
[@@deriving show { with_path = false }]

type expression =
  | Constant of value
  | Variable of modifier * identifier
  | Add of expression * expression
  | Sub of expression * expression
  | Mul of expression * expression
  | Div of expression * expression
  | Mod of expression * expression
  | Equal of expression * expression
  | Greater of expression * expression
  | GreaterOrEq of expression * expression
  | Less of expression * expression
  | LessOrEq of expression * expression
  | And of expression * expression
  | Or of expression * expression
  | List of expression list
  | Call of identifier * identifier * expression list
      (** (name of the parent: class OR "Null" in case of stand-alone func) (name of the called func) [expr list] *)
  | Lambda of expression list * statement list  (** [var list] [stmt list] *)
  | Nil  (** "null" *)
[@@deriving show { with_path = false }]

and statement =
  | Expression of expression
  | Assign of expression * expression
  | Return of expression
  | IfElse of expression * statement list * statement list
      (** (expr) [stmt list] [stmt list OR [] in case of "else" absence] *)
  | While of expression * statement list
  | Class of identifier * statement list
  | Function of identifier * expression list * statement list
      (** (id) [args list] [stmt list] *)
  | Break
  | Next  (** "continue" *)
[@@deriving show { with_path = false }]
