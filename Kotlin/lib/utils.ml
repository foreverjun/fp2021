open Ast
open Value_types

type parser_error =
  | InvalidProgram (** Возникает, если не удалось распарсить программу *)
[@@deriving show { with_path = false }]

type interpreter_error =
  | DivisionByZero of expression (** Возникает при попытке деления на 0 *)
  | ExpectedReturnInFunction of string
      (** Возникает, если у функции с типом, отличным от Unit, отсутствует return. Например, для функции [fun empty(): Int {}] при вызове выскочила бы эта ошибка*)
  | UnknownVariable of string
      (** Возникает, если в текущем контексте не объявлена переменная с именем string *)
  | UnknownFunction of string
      (** Возникает, если в текущем контексте не объявлена функция с именем string *)
  | UnknownClass of string
      (** Возникает, если в текущем контексте не объявлен класс с именем string *)
  | UnknownField of string * string
      (** Возникает, если в классе string не объявлено поле string *)
  | UnknownMethod of string * string
      (** Возникает, если в классе string не объявлен метод string *)
  | Redeclaration of string
      (** Возникает, если при объявлении новой сущности оказалось, что ее идентификатор string уже занят *)
  | ReturnNotInFunction (** Возникает, если return был вызван за пределами функции *)
  | FunctionArgumentsCountMismatch of string
      (** Возникает, если при вызове функции string в нее передано меньше аргументов, чем она принимает *)
  | UnsupportedTypeOfExpressionOnLeftSideOfAssign of expression
      (** Возникает, если слева от знака присваивания находится выражение, отличное от переменной или обращения к полю. Например, при попытке интерпретации [1 = 2] выпадет эта ошибка *)
  | ExpressionExpectedToBeNotNull of expression
      (** Возникает, если выражение expression интерпретируется в null, но при этом не должно так делать. В частности, если создать поле класса [val notNullable: Foo] и ничем ее не инициализировать, а потом попробовать обратиться к нему внутри init-блока [notNullable.bar], то выскачит эта ошибка. Это относится к данной ссылке https://counterexamples.org/under-construction.html*)
  | ExpectedObjectToDereference of expression
      (** Возникает, например, при попытке интерпретировать такой код 
      [val intVatiable: Int = 1
      intVariable.foo]
      , так как примитивный тип не является объектом (к сожалению) *)
  | ThisIsNotDefined
      (** Возникает при попытке обратиться с помощью this не внутри объекта *)
[@@deriving show { with_path = false }]

type typing_error =
  | VariableNotMutable of string
      (** Возникает, если попытаться присвоить не мутабельной переменной новое значение *)
  | ExpressionExpectedToBeNotNullable of expression
      (** Возникает, если согласно системе типов данное выражение не должно иметь nullable тип. В частности, это относится к обращению к переменным, функциям, методам и полям, тип которых объявлен как SomethingType?. Например, такая ошибка выпадет при выполнении такого кода
      [val nullable: Int? = 1
      val notNullable: Int = nullable] //здесь будет ошибка, так как в nullable потенциально мог лежать null *)
  | FunctionReturnTypeMismatch of string * typename * value
      (** Возникает, если функция string с типом typename вернула value, который не соответствует типу typename *)
  | VariableValueTypeMismatch of string * typename * value
      (** Возникает, если в переменную string с типом typename пытаются записать value, который не соответствует типу typename *)
  | ClassNotOpen of string
      (** Возникает при попытке наследования от класса string если он не помечен модификатором доступа open *)
  | PrivateAccessError of string * string
      (** Возникает, если попытаться обратиться к private или procted полю снаружи от объекта, в котором они объявлены. Также возникает, если попытаться обратиться к private полю супер класса *)
  | UnsupportedOperandTypes of expression
      (** Возникает, если при попытке применения некоторой операции (например, +, - или /) операнды имеют неподходящий тип *)
  | ExpectedBooleanValue of expression (* Возникает, если внутри [if(...)] или [while(...)] выражение не производит boolean *)
[@@deriving show { with_path = false }]

type error =
  | Parser of parser_error
  | Interpreter of interpreter_error
  | Typing of typing_error
[@@deriving show { with_path = false }]
