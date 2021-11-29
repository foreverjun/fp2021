(* Модификаторы, которые могут быть припианы перед именем класса\метода\поля *)
type modifier =
  | Private
  | Protected
  | Public
  | Open
  | Override
[@@deriving show]

(* Обязательный модификатор в котлин, который применяется только к полям*)
and variable_modifier =
  | Val
  | Var

(* Базовые типы *)
type typename =
  | Int
  | String
  | Boolean
  | Class of string
  | Nullable of typename
[@@deriving show]

(* Значения, которые могут принимать переменные *)
type value =
  | IntValue of int
  | StringValue of string
  | BooleanValue of bool
  | AnonymousFunction of function_t
  | NullValue
  | Unitialized

(* Переменная представляется как пара из имени переменной и значения *)
and variable_t =
  { var_typename : typename
  ; value : value
  }

and function_t =
  { fun_typename : typename
  ; arguments : (string * typename) list
  ; statement : statement
  }

and expression =
  | Add of expression * expression
  | Sub of expression * expression
  | Mul of expression * expression
  | Div of expression * expression
  | Mod of expression * expression
  | And of expression * expression
  | Or of expression * expression
  | Not of expression
  | Equal of expression * expression
  | Less of expression * expression
  | Const of value
  | VarIdentifier of string
  | ClassCreation of string * string list * expression list (* string<string list>(expression list) *)
  | MethodCall of string * string * expression list (* string.string(expression list) *)
  | FunctionCall of string * expression list
(* string(expression list) *)

and statement =
  | Return of expression
  | Expression of expression
  | Assign of string * expression (* string = expression *)
  | If of expression * statement * statement option (* if(expression) statement else statement *)
  | While of expression * statement (* while(expression) statement *)
  | VarDeclaration of
      modifier list * variable_modifier * string * typename * expression option (* modifiers variable_modifier string: typename = expression *)
  | FunDeclaration of
      modifier list * string * (string * typename) list * typename * statement (* modifiers fun string((string * typename) list): typename statement *)
  | ClassDeclaration of
      modifier list
      * string
      * string list
      * (modifier list option * variable_modifier option * string * typename * expression)
        list
      * statement (* modifiers string<string list>((modifier list option * variable_modifier option * string * typename * expression) list) statement*)
  | Block of statement list
[@@deriving show]
