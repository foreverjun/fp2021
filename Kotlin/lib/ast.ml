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
type value =
  | IntValue of int
  | StringValue of string
  | BooleanValue of bool
  | AnonymousFunction of function_t
  | Object of object_t
  | NullValue
  | Unitialized of object_t ref option

and record_content =
  | Variable of variable_t
  | Function of function_t
  | Class of class_t

and record_t =
  { name : string
  ; modifiers : modifier list
  ; clojure : (record_t list ref[@opaque])
  ; enclosing_object : (object_t option ref[@opaque])
  ; content : record_content
  }

and variable_t =
  { var_typename : typename
  ; mutable_status : bool
  ; value : value ref
  }

and function_t =
  { identity_code : int
  ; fun_typename : typename
  ; arguments : (string * typename) list
  ; statement : statement
  }

and object_t =
  { identity_code : int
  ; classname : string
  ; super : object_t option
  ; obj_class : class_t
  ; fields : record_t list
  ; methods : record_t list
  }

and class_t =
  { constructor_args : (string * typename) list
  ; super_call : expression option
        (** Выражение, содержащее содержащее вызов конструктора супер класса. Например, для class Foo(): Bar() будет super_call = FunctionCall ("Bar", []) *)
  ; statements : statement list
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
  | Const of value (** Например: 1, "string", false *)
  | VarIdentifier of string
      (** Строки string, не заключенные в кавычки, преставляются как VarIdentifier ("string") *)
  | This (** Специальное выражение для вызова this внутри объекта *)
  | AnonymousFunctionDeclaration of (string * typename) list * statement
      (** \{ (string * typename) list -> statement \}. Например: \{elem: Int -> elem * elem\} *)
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
  | VarDeclaration of
      modifier list * variable_modifier * string * typename * expression option
      (** modifiers variable_modifier string: typename = expression. Например: open val foo: String = "string" *)
  | FunDeclaration of
      modifier list * string * (string * typename) list * typename * statement
      (** modifiers fun string((string * typename) list): typename statement. Например: private fun foo(bar: Int): Int \{ return bar \} *)
  | ClassDeclaration of
      modifier list * string * (string * typename) list * expression option * statement
      (** modifiers class string(string * typename list): expression option statement. Например: open class Foo(bar: Int): Baz(bar) \{ ... \} *)
  | Block of statement list
      (** \{ statement list \} - набор выражений, окруженных фигурными скобками *)
  | InitializeBlock of statement list
      (** Вырожденный случай Block. Нужен только для функции parse_and_run, и по сути является Block без фигурных скобок вокруг *)
  | InitInClass of statement
      (** init \{ ... \}, причем данная конструкция должна встречаться только внутри классов *)
[@@deriving show { with_path = false }]
