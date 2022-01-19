open Ast
open Parser
open Value_types

(* Тесты на парсер *)

let%test _ =
  apply_parser parse_modifiers "public private protected open override"
  = Some [ Public; Private; Protected; Open; Override ]
;;

let%test _ = apply_parser parse_variable_type_modifier "var" = Some Var
let%test _ = apply_parser parse_variable_type_modifier "val" = Some Val

let%test _ =
  List.for_all
    (fun keyword -> apply_parser parse_identifier keyword = None)
    reserved_keywords
;;

let%test _ = apply_parser parse_identifier "identifier" = Some "identifier"
let%test _ = apply_parser parse_typename "Int" = Some Int
let%test _ = apply_parser parse_typename "String" = Some String
let%test _ = apply_parser parse_typename "Boolean" = Some Boolean
let%test _ = apply_parser parse_typename "MyClass" = Some (ClassIdentifier "MyClass")

let%test _ =
  apply_parser parse_typename "MyClass?" = Some (Nullable (ClassIdentifier "MyClass"))
;;

(* Тесты на парсер для Expression-ов *)
open Expression

let%test _ =
  List.for_all
    (fun keyword -> apply_parser parse_var_identifier keyword = None)
    reserved_keywords
;;

let%test _ =
  apply_parser parse_var_identifier "identifier" = Some (VarIdentifier "identifier")
;;

let%test _ = apply_parser parse_int_value "string" = None
let%test _ = apply_parser parse_int_value "123" = Some (IntValue 123)
let%test _ = apply_parser parse_int_value "-123" = Some (IntValue (-123))
let%test _ = apply_parser parse_string_value {| string" |} = None
let%test _ = apply_parser parse_string_value {| "string |} = None
let%test _ = apply_parser parse_string_value {| "string" |} = Some (StringValue "string")
let%test _ = apply_parser parse_string_value {| "" |} = Some (StringValue "")
let%test _ = apply_parser parse_boolean_value "false" = Some (BooleanValue false)
let%test _ = apply_parser parse_boolean_value "true" = Some (BooleanValue true)
let%test _ = apply_parser parse_null_value "null" = Some NullValue
let%test _ = apply_parser parse_const_value "string" = None
let%test _ = apply_parser parse_const_value "123" = Some (Const (IntValue 123))

let%test _ =
  apply_parser parse_const_value {| "string" |} = Some (Const (StringValue "string"))
;;

let%test _ = apply_parser parse_const_value "false" = Some (Const (BooleanValue false))
let%test _ = apply_parser parse_const_value "true" = Some (Const (BooleanValue true))
let%test _ = apply_parser parse_const_value "null" = Some (Const NullValue)

let%test _ =
  apply_parser expression {| 5 < 6 |}
  = Some (Less (Const (IntValue 5), Const (IntValue 6)))
;;

let%test _ =
  apply_parser expression {| 6 > 5 |}
  = Some (Less (Const (IntValue 5), Const (IntValue 6)))
;;

let%test _ =
  apply_parser expression {| 5 == 6 |}
  = Some (Equal (Const (IntValue 5), Const (IntValue 6)))
;;

let%test _ = apply_parser expression {| !true |} = Some (Not (Const (BooleanValue true)))

let%test _ =
  apply_parser expression {| 5 != 6 |}
  = Some (Not (Equal (Const (IntValue 5), Const (IntValue 6))))
;;

let%test _ =
  apply_parser expression {| 5 <= 6 |}
  = Some
      (Or
         ( Equal (Const (IntValue 5), Const (IntValue 6))
         , Less (Const (IntValue 5), Const (IntValue 6)) ))
;;

let%test _ =
  apply_parser expression {| 6 >= 5 |}
  = Some
      (Or
         ( Equal (Const (IntValue 6), Const (IntValue 5))
         , Less (Const (IntValue 5), Const (IntValue 6)) ))
;;

let%test _ =
  apply_parser expression {| !true || 6 >= 5 |}
  = Some
      (Or
         ( Not (Const (BooleanValue true))
         , Or
             ( Equal (Const (IntValue 6), Const (IntValue 5))
             , Less (Const (IntValue 5), Const (IntValue 6)) ) ))
;;

let%test _ =
  apply_parser expression {| (!true || 6 >= 5) && 1 < 2 |}
  = Some
      (And
         ( Or
             ( Not (Const (BooleanValue true))
             , Or
                 ( Equal (Const (IntValue 6), Const (IntValue 5))
                 , Less (Const (IntValue 5), Const (IntValue 6)) ) )
         , Less (Const (IntValue 1), Const (IntValue 2)) ))
;;

let%test _ =
  apply_parser expression {| !true || 6 + 1 > 5 && 1 < 2 |}
  = Some
      (Or
         ( Not (Const (BooleanValue true))
         , And
             ( Less (Const (IntValue 5), Add (Const (IntValue 6), Const (IntValue 1)))
             , Less (Const (IntValue 1), Const (IntValue 2)) ) ))
;;

let%test _ =
  apply_parser expression {| !true || 2 * 6 + 4 / 2 > 5 && 1 < 2 |}
  = Some
      (Or
         ( Not (Const (BooleanValue true))
         , And
             ( Less
                 ( Const (IntValue 5)
                 , Add
                     ( Mul (Const (IntValue 2), Const (IntValue 6))
                     , Div (Const (IntValue 4), Const (IntValue 2)) ) )
             , Less (Const (IntValue 1), Const (IntValue 2)) ) ))
;;

let%test _ =
  apply_parser expression {| { 1 } |}
  = Some (AnonymousFunctionDeclaration ([], [ Expression (Const (IntValue 1)) ]))
;;

let%test _ = apply_parser expression {| { -> 1 } |} = None

let%test _ =
  apply_parser expression {| { x: Int, y: Int -> x + y } |}
  = Some
      (AnonymousFunctionDeclaration
         ( [ "x", Int; "y", Int ]
         , [ Expression (Add (VarIdentifier "x", VarIdentifier "y")) ] ))
;;

let%test _ = apply_parser expression {| baz() |} = Some (FunctionCall ("baz", []))

let%test _ =
  apply_parser expression {| baz(foo, "bar") |}
  = Some (FunctionCall ("baz", [ VarIdentifier "foo"; Const (StringValue "bar") ]))
;;

let%test _ =
  apply_parser expression {| foo.bar?.baz() |}
  = Some
      (Dereference
         ( VarIdentifier "foo"
         , ElvisDereference (VarIdentifier "bar", FunctionCall ("baz", [])) ))
;;

(* Тесты на парсер для Statement-ов *)
open Statement

let%test _ =
  apply_parser statement {| { 1 } |} = Some (Block [ Expression (Const (IntValue 1)) ])
;;

let%test _ =
  apply_parser statement {| { 
    1 
    2
  } |}
  = Some (Block [ Expression (Const (IntValue 1)); Expression (Const (IntValue 2)) ])
;;

let%test _ =
  apply_parser statement {| fun main(): Int { 
    return 0
  } |}
  = Some
      (FunDeclaration
         { modifiers = []
         ; identifier = "main"
         ; args = []
         ; fun_typename = Int
         ; fun_statement = [ Return (Const (IntValue 0)) ]
         })
;;

let%test _ =
  apply_parser statement {| fun func_with_args(x: Int, y: Int): Int { 
    return 0
  } |}
  = Some
      (FunDeclaration
         { modifiers = []
         ; identifier = "func_with_args"
         ; args = [ "x", Int; "y", Int ]
         ; fun_typename = Int
         ; fun_statement = [ Return (Const (IntValue 0)) ]
         })
;;

let%test _ =
  apply_parser
    statement
    {| protected open fun func_with_modifiers(): Int { 
    return 0
  } |}
  = Some
      (FunDeclaration
         { modifiers = [ Protected; Open ]
         ; identifier = "func_with_modifiers"
         ; args = []
         ; fun_typename = Int
         ; fun_statement = [ Return (Const (IntValue 0)) ]
         })
;;

let%test _ =
  apply_parser statement {| val foo: Int |}
  = Some
      (VarDeclaration
         { modifiers = []
         ; var_modifier = Val
         ; identifier = "foo"
         ; var_typename = Int
         ; init_expression = None
         })
;;

let%test _ =
  apply_parser statement {| open val foo: Int = 1 |}
  = Some
      (VarDeclaration
         { modifiers = [ Open ]
         ; var_modifier = Val
         ; identifier = "foo"
         ; var_typename = Int
         ; init_expression = Some (Const (IntValue 1))
         })
;;

let%test _ =
  apply_parser statement {| class MyClass() { 
    
  } |}
  = Some (ClassDeclaration ([], "MyClass", [], None, []))
;;

let%test _ =
  apply_parser statement {| open class MyClassWithSuper(): Foo() { 
    
  } |}
  = Some (ClassDeclaration ([ Open ], "MyClassWithSuper", [], Some ("Foo", []), []))
;;

let%test _ =
  apply_parser
    statement
    {| class MyClassWithContent() { 
    var foo: Int = 1
    fun bar(): Int {
      return 0
    }
  } |}
  = Some
      (ClassDeclaration
         ( []
         , "MyClassWithContent"
         , []
         , None
         , [ VarDeclaration
               { modifiers = []
               ; var_modifier = Var
               ; identifier = "foo"
               ; var_typename = Int
               ; init_expression = Some (Const (IntValue 1))
               }
           ; FunDeclaration
               { modifiers = []
               ; identifier = "bar"
               ; args = []
               ; fun_typename = Int
               ; fun_statement = [ Return (Const (IntValue 0)) ]
               }
           ] ))
;;

let%test _ =
  apply_parser statement {| while(true) {} |}
  = Some (While (Const (BooleanValue true), Block []))
;;

let%test _ =
  apply_parser statement {| if(true) {} |}
  = Some (If (Const (BooleanValue true), Block [], None))
;;

let%test _ =
  apply_parser statement {| if(true) {} else {}|}
  = Some (If (Const (BooleanValue true), Block [], Some (Block [])))
;;

let%test _ =
  apply_parser statement {| if(true) 1 else 2|}
  = Some
      (If
         ( Const (BooleanValue true)
         , Block [ Expression (Const (IntValue 1)) ]
         , Some (Block [ Expression (Const (IntValue 2)) ]) ))
;;

let%test _ =
  apply_parser statement {| a = 1 |}
  = Some (Assign (VarIdentifier "a", Const (IntValue 1)))
;;

let%test _ =
  apply_parser statement {| a.foo = 1 * 2 + 3 |}
  = Some
      (Assign
         ( Dereference (VarIdentifier "a", VarIdentifier "foo")
         , Add (Mul (Const (IntValue 1), Const (IntValue 2)), Const (IntValue 3)) ))
;;

(* Тесты на интерпретатор *)
exception Test_failed

open Utils
open Interpreter
open Interpreter.Interpret

let ctx_with_standard_classes =
  Base.Option.value_exn (Base.Result.ok (load_standard_classes empty_ctx))
;;

let%test _ =
  let ctx = parse_and_run {| fun main(): Int { 
    return 1
  } 
  |} in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression
    = Primitive (IntValue 1)
;;

let%test _ =
  let ctx =
    interpret_expression
      ctx_with_standard_classes
      (Add (Const (IntValue 1), Const (IntValue 2)))
  in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression
    = Primitive (IntValue 3)
;;

let%test _ =
  let ctx =
    interpret_expression
      ctx_with_standard_classes
      (Add (Const (StringValue "foo"), Const (StringValue "bar")))
  in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression
    = Primitive (StringValue "foobar")
;;

let%test _ =
  let ctx =
    interpret_expression
      ctx_with_standard_classes
      (Add (Const (BooleanValue false), Const (StringValue "bar")))
  in
  match ctx with
  | Error err ->
    (match err with
    | Typing (UnsupportedOperandTypes _) -> true
    | _ -> false)
  | Ok _ -> raise Test_failed
;;

let%test _ =
  let ctx =
    interpret_expression
      ctx_with_standard_classes
      (Sub (Const (IntValue 1), Const (IntValue 2)))
  in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression
    = Primitive (IntValue (-1))
;;

let%test _ =
  let ctx =
    interpret_expression
      ctx_with_standard_classes
      (Sub (Const (StringValue "foo"), Const (StringValue "bar")))
  in
  match ctx with
  | Error err ->
    (match err with
    | Typing (UnsupportedOperandTypes _) -> true
    | _ -> false)
  | Ok _ -> raise Test_failed
;;

let%test _ =
  let ctx =
    interpret_expression
      ctx_with_standard_classes
      (Mul (Const (IntValue 1), Const (IntValue 2)))
  in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression
    = Primitive (IntValue 2)
;;

let%test _ =
  let ctx =
    interpret_expression
      ctx_with_standard_classes
      (Mul (Const (StringValue "foo"), Const (StringValue "bar")))
  in
  match ctx with
  | Error err ->
    (match err with
    | Typing (UnsupportedOperandTypes _) -> true
    | _ -> false)
  | Ok _ -> raise Test_failed
;;

let%test _ =
  let ctx =
    interpret_expression
      ctx_with_standard_classes
      (Div (Const (IntValue 2), Const (IntValue 2)))
  in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression
    = Primitive (IntValue 1)
;;

let%test _ =
  let ctx =
    interpret_expression
      ctx_with_standard_classes
      (Div (Const (IntValue 3), Const (IntValue 2)))
  in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression
    = Primitive (IntValue 1)
;;

let%test _ =
  let ctx =
    interpret_expression
      ctx_with_standard_classes
      (Mod (Const (IntValue 2), Const (IntValue 2)))
  in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression
    = Primitive (IntValue 0)
;;

let%test _ =
  let ctx =
    interpret_expression
      ctx_with_standard_classes
      (Mod (Const (IntValue 3), Const (IntValue 2)))
  in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression
    = Primitive (IntValue 1)
;;

let%test _ =
  let ctx =
    interpret_expression
      ctx_with_standard_classes
      (Div (Const (StringValue "foo"), Const (StringValue "bar")))
  in
  match ctx with
  | Error err ->
    (match err with
    | Typing (UnsupportedOperandTypes _) -> true
    | _ -> false)
  | Ok _ -> raise Test_failed
;;

let%test _ =
  let ctx =
    interpret_expression
      ctx_with_standard_classes
      (Mod (Const (StringValue "foo"), Const (StringValue "bar")))
  in
  match ctx with
  | Error err ->
    (match err with
    | Typing (UnsupportedOperandTypes _) -> true
    | _ -> false)
  | Ok _ -> raise Test_failed
;;

let%test _ =
  let ctx =
    interpret_expression
      ctx_with_standard_classes
      (And (Const (BooleanValue true), Const (BooleanValue false)))
  in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression
    = Primitive (BooleanValue false)
;;

let%test _ =
  let ctx =
    interpret_expression
      ctx_with_standard_classes
      (And (Const (BooleanValue false), Const (BooleanValue false)))
  in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression
    = Primitive (BooleanValue false)
;;

let%test _ =
  let ctx =
    interpret_expression
      ctx_with_standard_classes
      (And (Const (BooleanValue true), Const (BooleanValue true)))
  in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression
    = Primitive (BooleanValue true)
;;

let%test _ =
  let ctx =
    interpret_expression
      ctx_with_standard_classes
      (And (Const (StringValue "foo"), Const (StringValue "bar")))
  in
  match ctx with
  | Error err ->
    (match err with
    | Typing (UnsupportedOperandTypes _) -> true
    | _ -> false)
  | Ok _ -> raise Test_failed
;;

let%test _ =
  let ctx =
    interpret_expression
      ctx_with_standard_classes
      (Or (Const (BooleanValue true), Const (BooleanValue false)))
  in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression
    = Primitive (BooleanValue true)
;;

let%test _ =
  let ctx =
    interpret_expression
      ctx_with_standard_classes
      (Or (Const (BooleanValue false), Const (BooleanValue false)))
  in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression
    = Primitive (BooleanValue false)
;;

let%test _ =
  let ctx =
    interpret_expression
      ctx_with_standard_classes
      (Or (Const (BooleanValue true), Const (BooleanValue true)))
  in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression
    = Primitive (BooleanValue true)
;;

let%test _ =
  let ctx =
    interpret_expression
      ctx_with_standard_classes
      (Or (Const (StringValue "foo"), Const (StringValue "bar")))
  in
  match ctx with
  | Error err ->
    (match err with
    | Typing (UnsupportedOperandTypes _) -> true
    | _ -> false)
  | Ok _ -> raise Test_failed
;;

let%test _ =
  let ctx =
    interpret_expression ctx_with_standard_classes (Not (Const (BooleanValue false)))
  in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression
    = Primitive (BooleanValue true)
;;

let%test _ =
  let ctx =
    interpret_expression ctx_with_standard_classes (Not (Const (BooleanValue true)))
  in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression
    = Primitive (BooleanValue false)
;;

let%test _ =
  let ctx =
    interpret_expression ctx_with_standard_classes (Not (Const (StringValue "foo")))
  in
  match ctx with
  | Error err ->
    (match err with
    | Typing (UnsupportedOperandTypes _) -> true
    | _ -> false)
  | Ok _ -> raise Test_failed
;;

let%test _ =
  let ctx =
    interpret_expression
      ctx_with_standard_classes
      (Equal (Const (IntValue 1), Const (IntValue 1)))
  in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression
    = Primitive (BooleanValue true)
;;

let%test _ =
  let ctx =
    interpret_expression
      ctx_with_standard_classes
      (Equal (Const (IntValue 42), Const (IntValue 1)))
  in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression
    = Primitive (BooleanValue false)
;;

let%test _ =
  let ctx =
    interpret_expression
      ctx_with_standard_classes
      (Equal (Const (StringValue "foo"), Const (StringValue "foo")))
  in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression
    = Primitive (BooleanValue true)
;;

let%test _ =
  let ctx =
    interpret_expression
      ctx_with_standard_classes
      (Equal (Const (StringValue "foo"), Const (StringValue "bar")))
  in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression
    = Primitive (BooleanValue false)
;;

let%test _ =
  let ctx =
    interpret_expression
      ctx_with_standard_classes
      (Equal (Const (StringValue "foo"), Const (StringValue "foo")))
  in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression
    = Primitive (BooleanValue true)
;;

let%test _ =
  let ctx =
    interpret_expression
      ctx_with_standard_classes
      (Equal (Const (BooleanValue false), Const (BooleanValue true)))
  in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression
    = Primitive (BooleanValue false)
;;

let%test _ =
  let ctx =
    interpret_expression
      ctx_with_standard_classes
      (Equal (Const (BooleanValue true), Const (BooleanValue true)))
  in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression
    = Primitive (BooleanValue true)
;;

let%test _ =
  let ctx =
    interpret_expression
      ctx_with_standard_classes
      (Equal (Const NullValue, Const NullValue))
  in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression
    = Primitive (BooleanValue true)
;;

let%test _ =
  let var1 =
    { modifiers = []
    ; name = "var1"
    ; content =
        Variable
          { var_typename = FunctionType ([], Int)
          ; mutable_status = false
          ; value =
              AnonymousFunction
                { identity_code = 1
                ; fun_typename = Int
                ; clojure = []
                ; enclosing_object = None
                ; arguments = []
                ; statement = []
                }
          }
    }
  in
  let var2 =
    { modifiers = []
    ; name = "var2"
    ; content =
        Variable
          { var_typename = FunctionType ([], Int)
          ; mutable_status = false
          ; value =
              AnonymousFunction
                { identity_code = 2
                ; fun_typename = Int
                ; clojure = []
                ; enclosing_object = None
                ; arguments = []
                ; statement = []
                }
          }
    }
  in
  let ctx_with_vars =
    { ctx_with_standard_classes with environment = [ var1; var2 ], [] }
  in
  let ctx =
    interpret_expression
      ctx_with_vars
      (Equal (VarIdentifier "var1", VarIdentifier "var2"))
  in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression
    = Primitive (BooleanValue false)
;;

let%test _ =
  let ctx =
    interpret_expression
      ctx_with_standard_classes
      (Less (Const (IntValue 1), Const (IntValue 42)))
  in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression
    = Primitive (BooleanValue true)
;;

let%test _ =
  let ctx =
    interpret_expression
      ctx_with_standard_classes
      (Less (Const (IntValue 42), Const (IntValue 1)))
  in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression
    = Primitive (BooleanValue false)
;;

let%test _ =
  let ctx =
    interpret_expression
      ctx_with_standard_classes
      (Less (Const (BooleanValue false), Const (IntValue 1)))
  in
  match ctx with
  | Error err ->
    (match err with
    | Typing (UnsupportedOperandTypes _) -> true
    | _ -> false)
  | Ok _ -> raise Test_failed
;;

let%test _ =
  let ctx = interpret_expression ctx_with_standard_classes (Const (IntValue 1)) in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression
    = Primitive (IntValue 1)
;;

let%test _ =
  let ctx = interpret_expression ctx_with_standard_classes (Const (IntValue 1)) in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression
    = Primitive (IntValue 1)
;;

let%test _ =
  let ctx = interpret_expression ctx_with_standard_classes (Const (StringValue "foo")) in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression
    = Primitive (StringValue "foo")
;;

let%test _ =
  let ctx = interpret_expression ctx_with_standard_classes (Const NullValue) in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression = Primitive NullValue
;;

let%test _ =
  let content =
    { var_typename = Int; mutable_status = false; value = Primitive (IntValue 1) }
  in
  let rc = { name = "foo"; modifiers = []; content = Variable content } in
  let ctx_with_variable = { ctx_with_standard_classes with environment = [ rc ], [] } in
  let ctx = interpret_expression ctx_with_variable (VarIdentifier "foo") in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    (match eval_ctx.last_eval_expression with
    | VariableRecord (rc1, var)
      when var.value = Primitive (IntValue 1) && (rc1, var) = (rc, content) -> true
    | _ -> raise Test_failed)
;;

let%test _ =
  let ctx = interpret_expression ctx_with_standard_classes (VarIdentifier "foo") in
  match ctx with
  | Error err ->
    (match err with
    | Interpreter (UnknownVariable "foo") -> true
    | _ -> raise Test_failed)
  | Ok _ -> raise Test_failed
;;

let%test _ =
  let rc =
    { name = "foo"
    ; modifiers = []
    ; content =
        Function
          { identity_code = 1
          ; fun_typename = Int
          ; clojure = []
          ; enclosing_object = None
          ; arguments = []
          ; statement = [ Return (Const (IntValue 1)) ]
          }
    }
  in
  let ctx_with_function = { ctx_with_standard_classes with environment = [ rc ], [] } in
  let ctx = interpret_expression ctx_with_function (FunctionCall ("foo", [])) in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression
    = Primitive (IntValue 1)
;;

let%test _ =
  let ctx = interpret_expression ctx_with_standard_classes (FunctionCall ("foo", [])) in
  match ctx with
  | Error err ->
    (match err with
    | Interpreter (UnknownFunction "foo") -> true
    | _ -> raise Test_failed)
  | Ok _ -> raise Test_failed
;;

let%test _ =
  let rc =
    { name = "foo"
    ; modifiers = []
    ; content =
        Variable
          { var_typename = ClassIdentifier "MyClass"
          ; mutable_status = false
          ; value =
              Object
                { identity_code = 1
                ; super = None
                ; obj_class =
                    { classname = "MyClass"
                    ; clojure = []
                    ; constructor_args = []
                    ; super_constructor = None
                    ; field_initializers = []
                    ; method_initializers = []
                    ; init_statements = []
                    }
                ; fields =
                    [ { name = "field"
                      ; modifiers = []
                      ; content =
                          Variable
                            { var_typename = Int
                            ; mutable_status = false
                            ; value = Primitive (IntValue 1)
                            }
                      }
                    ]
                ; methods = []
                }
          }
    }
  in
  let ctx_with_object = { ctx_with_standard_classes with environment = [ rc ], [] } in
  let ctx =
    interpret_expression
      ctx_with_object
      (Dereference (VarIdentifier "foo", VarIdentifier "field"))
  in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression
    = Primitive (IntValue 1)
;;

let%test _ =
  let rc =
    { name = "foo"
    ; modifiers = []
    ; content =
        Variable
          { var_typename = Int; mutable_status = false; value = Primitive NullValue }
    }
  in
  let ctx_with_object = { ctx_with_standard_classes with environment = [ rc ], [] } in
  let ctx =
    interpret_expression
      ctx_with_object
      (ElvisDereference (VarIdentifier "foo", VarIdentifier "field"))
  in
  match ctx with
  | Error _ -> raise Test_failed
  | Ok eval_ctx ->
    get_value_of_evaluated_expression eval_ctx.last_eval_expression = Primitive NullValue
;;

(*nullable тест*)
let%test _ =
  let content =
    { var_typename = Int; mutable_status = false; value = Primitive (IntValue 1) }
  in
  let rc = { name = "foo"; modifiers = []; content = Variable content } in
  let ctx_with_variable = { ctx_with_standard_classes with environment = [ rc ], [] } in
  match check_expression_is_nullable ctx_with_variable (VarIdentifier "foo") with
  | Ok flag when Bool.equal flag false -> true
  | _ -> raise Test_failed
;;

let%test _ =
  let content =
    { var_typename = Nullable Int
    ; mutable_status = false
    ; value = Primitive (IntValue 1)
    }
  in
  let rc = { name = "foo"; modifiers = []; content = Variable content } in
  let ctx_with_variable = { ctx_with_standard_classes with environment = [ rc ], [] } in
  match check_expression_is_nullable ctx_with_variable (VarIdentifier "foo") with
  | Ok flag when Bool.equal flag true -> true
  | _ -> raise Test_failed
;;

let%test _ =
  let rc =
    { name = "foo"
    ; modifiers = []
    ; content =
        Function
          { identity_code = 1
          ; fun_typename = Int
          ; clojure = []
          ; enclosing_object = None
          ; arguments = []
          ; statement = [ Return (Const (IntValue 1)) ]
          }
    }
  in
  let ctx_with_function = { ctx_with_standard_classes with environment = [ rc ], [] } in
  match check_expression_is_nullable ctx_with_function (FunctionCall ("foo", [])) with
  | Ok flag when Bool.equal flag false -> true
  | _ -> raise Test_failed
;;

let%test _ =
  let rc =
    { name = "foo"
    ; modifiers = []
    ; content =
        Function
          { identity_code = 1
          ; fun_typename = Nullable Int
          ; clojure = []
          ; enclosing_object = None
          ; arguments = []
          ; statement = [ Return (Const (IntValue 1)) ]
          }
    }
  in
  let ctx_with_function = { ctx_with_standard_classes with environment = [ rc ], [] } in
  match check_expression_is_nullable ctx_with_function (FunctionCall ("foo", [])) with
  | Ok flag when Bool.equal flag true -> true
  | _ -> raise Test_failed
;;

let%test _ =
  let my_class =
    { classname = "MyClass"
    ; clojure = []
    ; constructor_args = []
    ; super_constructor = None
    ; field_initializers =
        [ { modifiers = []
          ; var_modifier = Val
          ; identifier = "field"
          ; var_typename = Int
          ; init_expression = Some (Const (IntValue 1))
          }
        ]
    ; method_initializers = []
    ; init_statements = []
    }
  in
  let rc =
    { name = "myclass_instance"
    ; modifiers = []
    ; content =
        Variable
          { var_typename = ClassIdentifier "MyClass"
          ; mutable_status = false
          ; value =
              Object
                { identity_code = 1
                ; super = None
                ; obj_class = my_class
                ; fields =
                    [ { name = "foo"
                      ; modifiers = []
                      ; content =
                          Variable
                            { var_typename = Int
                            ; mutable_status = false
                            ; value = Primitive (IntValue 1)
                            }
                      }
                    ]
                ; methods = []
                }
          }
    }
  in
  let ctx_with_object = { ctx_with_standard_classes with environment = [ rc ], [] } in
  match
    check_expression_is_nullable
      ctx_with_object
      (Dereference (VarIdentifier "myclass_instance", VarIdentifier "field"))
  with
  | Ok flag when Bool.equal flag false -> true
  | _ -> raise Test_failed
;;

let%test _ =
  let my_class =
    { classname = "MyClass"
    ; clojure = []
    ; constructor_args = []
    ; super_constructor = None
    ; field_initializers =
        [ { modifiers = []
          ; var_modifier = Val
          ; identifier = "field"
          ; var_typename = Nullable Int
          ; init_expression = Some (Const (IntValue 1))
          }
        ]
    ; method_initializers = []
    ; init_statements = []
    }
  in
  let rc =
    { name = "myclass_instance"
    ; modifiers = []
    ; content =
        Variable
          { var_typename = ClassIdentifier "MyClass"
          ; mutable_status = false
          ; value =
              Object
                { identity_code = 1
                ; super = None
                ; obj_class = my_class
                ; fields =
                    [ { name = "foo"
                      ; modifiers = []
                      ; content =
                          Variable
                            { var_typename = Nullable Int
                            ; mutable_status = false
                            ; value = Primitive (IntValue 1)
                            }
                      }
                    ]
                ; methods = []
                }
          }
    }
  in
  let ctx_with_object = { ctx_with_standard_classes with environment = [ rc ], [] } in
  match
    check_expression_is_nullable
      ctx_with_object
      (Dereference (VarIdentifier "myclass_instance", VarIdentifier "field"))
  with
  | Ok flag when Bool.equal flag true -> true
  | _ -> raise Test_failed
;;

let%test _ =
  let my_class =
    { classname = "MyClass"
    ; clojure = []
    ; constructor_args = []
    ; super_constructor = None
    ; field_initializers =
        [ { modifiers = []
          ; var_modifier = Val
          ; identifier = "field"
          ; var_typename = Nullable Int
          ; init_expression = Some (Const (IntValue 1))
          }
        ]
    ; method_initializers = []
    ; init_statements = []
    }
  in
  let rc =
    { name = "myclass_instance"
    ; modifiers = []
    ; content =
        Variable
          { var_typename = Nullable (ClassIdentifier "MyClass")
          ; mutable_status = false
          ; value =
              Object
                { identity_code = 1
                ; super = None
                ; obj_class = my_class
                ; fields =
                    [ { name = "foo"
                      ; modifiers = []
                      ; content =
                          Variable
                            { var_typename = Int
                            ; mutable_status = false
                            ; value = Primitive (IntValue 1)
                            }
                      }
                    ]
                ; methods = []
                }
          }
    }
  in
  let ctx_with_object = { ctx_with_standard_classes with environment = [ rc ], [] } in
  match
    check_expression_is_nullable
      ctx_with_object
      (Dereference (VarIdentifier "myclass_instance", VarIdentifier "field"))
  with
  | Ok flag when Bool.equal flag true -> true
  | _ -> raise Test_failed
;;
