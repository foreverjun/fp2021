open Base
open Ast

type error =
  | EmptyProgram
  | VariableNotMutable of string
  | ExpectedReturnInFunction of string
  | ThisExpressionError
  | UnsupportedOperandTypes of expression
  | NullUnsafeAccessError
  | DereferenceError
  | NullableDereferenceError
  | FunctionReturnTypeMismatch of string * typename * value
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
  | ExpectedObjectToDereference
  | ClassNotOpen of string
  | PrivateAccessError of string * string
  | UnknownField of string * string
  | UnknownMethod of string * string
[@@deriving show]
