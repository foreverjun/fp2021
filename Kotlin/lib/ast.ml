(* Модификаторы, которые могут быть припианы перед именем класса\метода\поля *)
type modifier =
  | Private
  | Protected
  | Public
  | Open
  | Override

(* Обязательный модификатор в котлин, который применяется только к полям*)
and variable_modifier =
  | Val
  | Var

(* Базовые типы *)
and typename =
  | Int
  | String
  | Boolean
  | ClassIdentifier of string
  | FunctionType of typename list * typename
  | Dynamic (* данный тип по большей части кастыль, так как не удалось сделать алгоритм выведения типа для анонимной функции *)
  | Nullable of typename

(* Значения, которые могут принимать переменные *)
and value =
  | IntValue of int
  | StringValue of string
  | BooleanValue of bool
  | AnonymousFunction of function_t
  | Object of object_t
  | NullValue
  | Unitialized

and record_content =
  | Variable of variable_t
  | Function of function_t
  | Class of class_t

and record_t =
  { name : string
  ; modifiers : modifier list
  ; clojure : (record_t list ref[@opaque])
  ; content : record_content
  }

(* Переменная представляется как пара из имени переменной и значения *)
and variable_t =
  { var_typename : typename
  ; mutable_status : bool
  ; value : value ref
  }

and function_t =
  { fun_typename : typename
  ; arguments : (string * typename) list
  ; statement : statement
  }

and object_t =
  { classname : string
  ; super : object_t option
  ; obj_class : class_t
  ; fields : record_t list
  ; methods : record_t list
  }

and class_t =
  { constructor_args : (string * typename) list
  ; super_call : expression option
  ; statements : statement list
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
  | AnonymousFunctionDeclaration of statement
  | FunctionCall of string * expression list
  | Println of expression (* println(expression) *)
  | Dereference of expression * expression
  | ElvisDereference of expression * expression
(* expression.expression where expression = FunctionCall | VarIdentifier *)

and statement =
  | Return of expression
  | Expression of expression
  | Assign of expression * expression (* string = expression *)
  | If of expression * statement * statement option (* if(expression) statement else statement *)
  | While of expression * statement (* while(expression) statement *)
  | VarDeclaration of
      modifier list * variable_modifier * string * typename * expression option (* modifiers variable_modifier string: typename = expression *)
  | FunDeclaration of
      modifier list * string * (string * typename) list * typename * statement (* modifiers fun string((string * typename) list): typename statement *)
  (* пофиксить проблему с тем, что тип функции может быть пустым *)
  | ClassDeclaration of
      modifier list * string * (string * typename) list * expression option * statement (* modifiers class string(string * typename list): expression option statement*)
  | Block of statement list
  | InitializeBlock of statement list
  | AnonymousFunctionDeclarationStatement of (string * typename) list * statement
[@@deriving show { with_path = false }]
