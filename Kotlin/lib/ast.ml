(** Модификаторы доступа *)
type modifier =
  | Private
  | Protected
  | Public
  | Open
  | Override
[@@deriving show { with_path = false }]

(** Модификатор, указывающий на изменяемость переменной ([val] - неизменяемая, [var] - изменяемая)*)
type variable_modifier =
  | Val
  | Var
[@@deriving show { with_path = false }]

(** Базовые типы *)
type typename =
  | Unit
  | Int
  | String
  | Boolean
  | ClassIdentifier of string
  | FunctionType of typename list * typename
  | Dynamic
      (** данный тип по большей части костыль, так как не удалось сделать алгоритм выведения типа для анонимной функции *)
  | Nullable of typename
      (** если переменная некоторого типа typename может содержать null, то она обязана быть typename = Nullable _ *)
[@@deriving show { with_path = false }]

(** Значения, которые могут принимать переменные *)
type primitive_value =
  | IntValue of int
  | StringValue of string
  | BooleanValue of bool
  | NullValue
[@@deriving show { with_path = false }]

type var_initializer =
  { modifiers : modifier list
  ; var_modifier : variable_modifier
  ; identifier : string
  ; var_typename : typename
  ; init_expression : expression option
  }

and fun_initializer =
  { modifiers : modifier list
  ; identifier : string
  ; args : (string * typename) list
  ; fun_typename : typename
  ; fun_statement : statement list
  }

and expression =
  | Add of expression * expression (** expression + expression *)
  | Sub of expression * expression (** expression - expression *)
  | Mul of expression * expression (** expression * expression *)
  | Div of expression * expression (** expression / expression *)
  | Mod of expression * expression (** expression % expression *)
  | And of expression * expression (** expression && expression *)
  | Or of expression * expression (** expression || expression *)
  | Not of expression (** !expression *)
  | Equal of expression * expression (** expression == expression *)
  | Less of expression * expression (** expression < expression *)
  | Const of primitive_value (** Например: 1, "string", false *)
  | VarIdentifier of string
      (** Строки string, не заключенные в кавычки, преставляются как VarIdentifier ("string") *)
  | This (** Специальное выражение для вызова this внутри объекта *)
  | AnonymousFunctionDeclaration of (string * typename) list * statement list
      (** \{ (string * typename) list -> statement \}. Например: \{elem: Int -> elem * elem\} - анонимная функция для возведения числа в квадрат*)
  | FunctionCall of string * expression list
      (** Например: foo(bar) <=> FunctionCall ("foo", \[VarIdentifier "bar"\])*)
  | Println of expression (** println(expression) *)
  | Dereference of expression * expression
      (** expression.expression где expression = FunctionCall | VarIdentifier *)
  | ElvisDereference of expression * expression (** expression?.expression  *)

and statement =
  | Return of expression (** return expression *)
  | Expression of expression (** expression *)
  | Assign of expression * expression (** expression = expression *)
  | If of expression * statement * statement option
      (** if(expression) statement else statement, причем statement = Block*)
  | While of expression * statement
      (** while(expression) statement, причем statement = Block *)
  | VarDeclaration of var_initializer
      (** modifiers variable_modifier string: typename = expression. Например: open val foo: String = "string" *)
  | FunDeclaration of fun_initializer
      (** modifiers fun string((string * typename) list): typename statement. Например: private fun foo(bar: Int): Int \{ return bar \} *)
  | ClassDeclaration of
      modifier list
      * string
      * (string * typename) list
      * (string * expression list) option
      * statement list
      (** modifiers class string(string * typename list): string(expression list) {statement list}. Например: open class Foo(bar: Int): Baz(bar) \{ ... \} *)
  | Block of statement list
      (** \{ statement list \} - набор выражений, окруженных фигурными скобками *)
  | InitInClass of statement
      (** init \{ ... \}, причем данная конструкция должна встречаться только внутри классов *)
[@@deriving show { with_path = false }]
