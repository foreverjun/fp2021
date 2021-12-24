open Base
open Ast

type error =
  | EmptyProgram
  | UnknownVariable of string
  | UnknownFunction of string
  | Redeclaration of string
  | VariableTypeMismatch of string
  | VariableValueTypeMismatch of string * typename * value
  | ExpectedFunctionButFoundVariable of string
  | ReturnNotInFunction
  | FunctionBodyExpected
  | FunctionArgumentsCountMismatch
  | ExpectedBooleanValue
  | ClassBodyExpected
  | ClassSuperConstructorNotValid
  | ExpectedVarIdentifer
  | ExprectedObjectToDereference
  | PrivateAccessError of string * string
  | UnknownField of string * string (* первый элемент в паре - имя класса, второй - имя поля *)
(* Кидаем если пользователь хотел переменную, а по такому имени в окружении записана функция *)
[@@deriving show]

module type MONAD_FAIL = sig
  include Base.Monad.S2

  val fail : 'e -> ('a, 'e) t
end
