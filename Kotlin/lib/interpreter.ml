open Base
open Utils
open Parser
open Ast

module Interpret (M : MONAD_FAIL) = struct
  open M

  type scope_type =
    | Initialize
    | Function
    | Object of object_t ref
    | Method of object_t ref

  (* стоит добавить вложенность environment-ов на локальный и глоабльный, чтобы ввести понятие глобальных переменных *)
  type context =
    { environment : record_t list
    ; last_eval_expr : value
    ; last_return_value : value
    ; last_derefered_variable : variable_t option
    ; scope : scope_type
    }

  let check_typename_value_correspondance = function
    | (Int | Nullable Int), IntValue _ -> true
    | (String | Nullable String), StringValue _ -> true
    | (Boolean | Nullable Boolean), BooleanValue _ -> true
    | Nullable _, NullValue -> true
    | _, _ -> false
  ;;

  let check_record_is_private rc =
    Option.is_some
      (List.find_map rc.modifiers ~f:(function
          | Private -> Some Private
          | _ -> None))
  ;;

  let rec get_var_from_env env name =
    let filtered_env =
      List.filter env ~f:(fun r ->
          match r.content with
          | Variable _ -> true
          | _ -> false)
    in
    List.find_map filtered_env ~f:(fun r ->
        if String.equal r.name name then Some r else None)
  ;;

  let rec get_function_or_class_from_env env name =
    let filtered_env =
      List.filter env ~f:(fun r ->
          match r.content with
          | Function _ -> true
          | Class _ -> true
          | _ -> false)
    in
    List.find_map filtered_env ~f:(fun r ->
        if String.equal r.name name then Some r else None)
  ;;

  let rec get_class_from_env env name =
    let filtered_env =
      List.filter env ~f:(fun r ->
          match r.content with
          | Class _ -> true
          | _ -> false)
    in
    List.find_map filtered_env ~f:(fun r ->
        if String.equal r.name name then Some r else None)
  ;;

  let define_in_ctx ctx rc =
    match rc.content with
    | Variable var ->
      let var_names =
        List.filter_map ctx.environment ~f:(function rc ->
            (match rc.content with
            | Variable v -> Some rc.name
            | _ -> None))
      in
      if not (List.mem var_names rc.name ~equal:String.equal)
      then (
        let new_environment = rc :: ctx.environment in
        rc.clojure := new_environment;
        Some { ctx with environment = new_environment })
      else None
    | Function func ->
      let func_names =
        List.filter_map ctx.environment ~f:(function rc ->
            (match rc.content with
            | Function f -> Some rc.name
            | _ -> None))
      in
      if not (List.mem func_names rc.name ~equal:String.equal)
      then (
        let new_environment = rc :: ctx.environment in
        rc.clojure := new_environment;
        Some { ctx with environment = new_environment })
      else None
    | Class cls ->
      let cls_names =
        List.filter_map ctx.environment ~f:(function rc ->
            (match rc.content with
            | Class c -> Some rc.name
            | _ -> None))
      in
      if not (List.mem cls_names rc.name ~equal:String.equal)
      then (
        let new_environment = rc :: ctx.environment in
        rc.clojure := new_environment;
        Some { ctx with environment = new_environment })
      else None
  ;;

  let set_var_in_ctx ctx name value =
    match get_var_from_env ctx.environment name with
    | None -> None
    | Some rc ->
      (match rc.content with
      | Variable content ->
        content.value := value;
        Some rc
      | _ -> failwith "Should not reach here")
  ;;

  let rec get_field_from_object obj name =
    match
      List.find_map obj.fields ~f:(fun r ->
          if String.equal r.name name then Some r else None)
    with
    | Some r -> Some r
    | None ->
      (match obj.super with
      | None -> None
      | Some super ->
        (match get_field_from_object super name with
        | None -> None
        | Some r_super -> if check_record_is_private r_super then None else Some r_super))
  ;;

  let rec get_method_from_object obj name =
    match
      List.find_map obj.methods ~f:(fun r ->
          if String.equal r.name name then Some r else None)
    with
    | Some r -> Some r
    | None ->
      (match obj.super with
      | None -> None
      | Some super ->
        (match get_method_from_object super name with
        | None -> None
        | Some r_super -> if check_record_is_private r_super then None else Some r_super))
  ;;

  let define_in_object obj rc =
    match rc.content with
    | Variable var ->
      let defined_fields = List.map obj.fields ~f:(fun o -> o.name) in
      if not (List.mem defined_fields rc.name ~equal:String.equal)
      then (
        let new_fields = rc :: obj.fields in
        Some { obj with fields = new_fields })
      else None
    | Function func ->
      let defined_methods = List.map obj.methods ~f:(fun o -> o.name) in
      if not (List.mem defined_methods rc.name ~equal:String.equal)
      then (
        let new_methods = rc :: obj.methods in
        Some { obj with methods = new_methods })
      else None
    | Class cls -> failwith "Not yet implemented"
  ;;

  let rec run (ctx : context) statement = interpret_statement ctx statement

  and interpret_statement ctx = function
    | Block statements ->
      List.fold statements ~init:(M.return ctx) ~f:(fun monadic_ctx stat ->
          monadic_ctx
          >>= fun checked_ctx ->
          interpret_statement checked_ctx stat
          >>= fun eval_ctx ->
          match eval_ctx.last_return_value with
          | Unitialized ->
            (match stat with
            | Block _ -> M.return checked_ctx
            | _ -> M.return eval_ctx)
          | _ ->
            (match checked_ctx.scope with
            | Function | Method _ -> M.return eval_ctx
            | _ -> M.fail ReturnNotInFunction))
    | Return expression ->
      interpret_expression ctx expression
      >>= fun ret_ctx -> M.return { ctx with last_return_value = ret_ctx.last_eval_expr }
    | Expression expression -> interpret_expression ctx expression
    (* TODO: разобраться с модификаторами у классов *)
    | ClassDeclaration (modifiers, name, constructor_args, super_call, class_statement) ->
      (match class_statement with
      | Block statements -> M.return statements
      | _ -> M.fail ClassBodyExpected)
      >>= fun statements ->
      (match
         define_in_ctx
           ctx
           { name
           ; modifiers
           ; clojure = ref ctx.environment
           ; content = Class { constructor_args; super_call; statements }
           }
       with
      | None -> M.fail (Redeclaration name)
      | Some new_ctx -> M.return new_ctx)
    (* TODO: разобраться с модификаторами у функций *)
    | FunDeclaration (modifiers, name, arguments, fun_typename, fun_statement) ->
      (match fun_statement with
      | Block _ -> M.return fun_statement
      | _ -> M.fail FunctionBodyExpected)
      >>= fun statement ->
      (match
         define_in_ctx
           ctx
           { name
           ; modifiers
           ; clojure = ref ctx.environment
           ; content = Function { fun_typename; arguments; statement }
           }
       with
      | None -> M.fail (Redeclaration name)
      | Some new_ctx -> M.return new_ctx)
    (* TODO: разобраться с модификаторами у переменных *)
    | VarDeclaration (modifiers, var_modifier, name, var_typename, init_expression) ->
      (match init_expression with
      | None -> M.return Unitialized
      | Some expr ->
        interpret_expression ctx expr
        >>= fun interpreted_ctx -> M.return interpreted_ctx.last_eval_expr)
      >>= fun value ->
      (match
         define_in_ctx
           ctx
           { name
           ; modifiers
           ; clojure = ref ctx.environment
           ; content =
               Variable
                 { var_typename; mutable_status = var_modifier == Var; value = ref value }
           }
       with
      | None -> M.fail (Redeclaration name)
      | Some new_ctx -> M.return new_ctx)
    (* исправить *)
    | Assign (var_identifier_expression, assign_expression) ->
      interpret_expression ctx var_identifier_expression
      >>= fun identifier_ctx ->
      (match identifier_ctx.last_derefered_variable with
      | None -> M.fail ExpectedVarIdentifer
      | Some var ->
        interpret_expression ctx assign_expression
        >>= fun assign_value_ctx ->
        if var.mutable_status
           && check_typename_value_correspondance
                (var.var_typename, assign_value_ctx.last_eval_expr)
        then (
          var.value := assign_value_ctx.last_eval_expr;
          M.return assign_value_ctx)
        else
          M.fail
            (VariableValueTypeMismatch
               ( "FIXME: тут должно быть имя переменной"
               , var.var_typename
               , assign_value_ctx.last_eval_expr )))
    | If (log_expr, if_statement, else_statement) ->
      interpret_expression ctx log_expr
      >>= fun eval_ctx ->
      (match eval_ctx.last_eval_expr with
      | BooleanValue log_val ->
        if log_val
        then interpret_statement ctx if_statement
        else (
          match else_statement with
          | None -> M.return ctx
          | Some stat -> interpret_statement ctx stat)
      | _ -> M.fail ExpectedBooleanValue)
    | _ -> failwith "not implemented statement type"

  and interpret_expression ctx =
    let eval_bin_args l r =
      interpret_expression ctx l
      >>= fun l_ctx ->
      interpret_expression ctx r
      >>= fun r_ctx -> return (l_ctx.last_eval_expr, r_ctx.last_eval_expr)
    in
    let eval_un_arg x =
      interpret_expression ctx x >>= fun x_ctx -> return x_ctx.last_eval_expr
    in
    function
    | FunctionCall (identifier, arg_expressions) ->
      let define_args args_ctx args exprs =
        List.fold2
          args
          exprs
          ~init:(M.return args_ctx)
          ~f:(fun monadic_ctx (arg_name, arg_typename) arg_expr ->
            monadic_ctx
            >>= fun monadic_ctx ->
            interpret_expression ctx arg_expr
            >>= fun eval_expr_ctx ->
            if check_typename_value_correspondance
                 (arg_typename, eval_expr_ctx.last_eval_expr)
            then (
              match
                define_in_ctx
                  monadic_ctx
                  { name = arg_name
                  ; modifiers = []
                  ; clojure = ref ctx.environment
                  ; content =
                      Variable
                        { var_typename = arg_typename
                        ; mutable_status = false
                        ; value = ref eval_expr_ctx.last_eval_expr
                        }
                  }
              with
              | None -> M.fail (Redeclaration arg_name)
              | Some ctx -> M.return ctx)
            else M.fail (VariableTypeMismatch arg_name))
      in
      (match ctx.scope with
      | Object obj | Method obj ->
        (match get_method_from_object !obj identifier with
        | None -> M.fail (UnknownVariable identifier)
        | Some meth ->
          let meth_content =
            match meth.content with
            | Function f -> f
            | _ -> failwith "should not reach here"
          in
          let new_ctx = { ctx with environment = !(meth.clojure); scope = Method obj } in
          (match define_args new_ctx meth_content.arguments arg_expressions with
          | Ok fun_ctx ->
            fun_ctx
            >>= fun fun_ctx ->
            interpret_statement fun_ctx meth_content.statement
            >>= fun func_eval_ctx ->
            M.return { ctx with last_eval_expr = func_eval_ctx.last_return_value }
          | _ -> M.fail FunctionArgumentsCountMismatch))
      | _ ->
        (match get_function_or_class_from_env ctx.environment identifier with
        | None -> M.fail (UnknownFunction identifier)
        | Some rc ->
          (match rc.content with
          | Variable _ -> failwith "should not reach here"
          | Class class_data ->
            let new_object =
              ref { super = None; obj_class = class_data; fields = []; methods = [] }
            in
            let new_ctx = { ctx with environment = !(rc.clojure); scope = Function } in
            (match define_args new_ctx class_data.constructor_args arg_expressions with
            | Ok constructor_ctx ->
              constructor_ctx
              >>= fun class_ctx ->
              (match class_data.super_call with
              | None -> M.return class_ctx
              | Some expr ->
                interpret_expression class_ctx expr
                >>= fun sup_ctx ->
                M.return sup_ctx.last_eval_expr
                >>= (function
                | Object super_object ->
                  new_object := { !new_object with super = Some super_object };
                  (* пофиксить: добавить вложенность супера в объект *)
                  M.return class_ctx
                | _ -> M.fail ClassSuperConstructorNotValid))
              >>= fun evaluated_constructor_ctx ->
              M.return { evaluated_constructor_ctx with scope = Object new_object }
              >>= fun class_inner_ctx ->
              List.fold
                class_data.statements
                ~init:(M.return class_inner_ctx)
                ~f:(fun acc stat ->
                  match stat with
                  | VarDeclaration
                      (modifiers, var_modifier, name, var_typename, init_expression) ->
                    (match init_expression with
                    | None -> M.return Unitialized
                    | Some expr ->
                      interpret_expression class_inner_ctx expr
                      >>= fun interpreted_ctx -> M.return interpreted_ctx.last_eval_expr)
                    >>= fun value ->
                    (match
                       define_in_object
                         !new_object
                         { name
                         ; modifiers
                         ; clojure = ref ctx.environment
                         ; content =
                             Variable
                               { var_typename; mutable_status = false; value = ref value }
                         }
                     with
                    | None -> M.fail (Redeclaration name)
                    | Some updated_obj ->
                      new_object := updated_obj;
                      M.return class_inner_ctx)
                  | FunDeclaration
                      (modifiers, name, arguments, fun_typename, fun_statement) ->
                    (match fun_statement with
                    | Block _ -> M.return fun_statement
                    | _ -> M.fail FunctionBodyExpected)
                    >>= fun statement ->
                    (match
                       define_in_object
                         !new_object
                         { name
                         ; modifiers
                         ; clojure = ref ctx.environment
                         ; content = Function { fun_typename; arguments; statement }
                         }
                     with
                    | None -> M.fail (Redeclaration name)
                    | Some updated_obj ->
                      new_object := updated_obj;
                      M.return class_inner_ctx)
                  | _ -> failwith "Not yet implemented")
              >>= fun _ -> M.return { ctx with last_eval_expr = Object !new_object }
            | _ -> M.fail FunctionArgumentsCountMismatch)
          | Function func ->
            let new_ctx = { ctx with environment = !(rc.clojure); scope = Function } in
            (match define_args new_ctx func.arguments arg_expressions with
            | Ok fun_ctx ->
              fun_ctx
              >>= fun fun_ctx ->
              interpret_statement fun_ctx func.statement
              >>= fun func_eval_ctx ->
              M.return { ctx with last_eval_expr = func_eval_ctx.last_return_value }
            | _ -> M.fail FunctionArgumentsCountMismatch))))
    | Const x -> M.return { ctx with last_eval_expr = x }
    | VarIdentifier identifier ->
      (match ctx.scope with
      | Object obj | Method obj ->
        if String.equal identifier "this"
        then M.return { ctx with last_eval_expr = Object !obj }
        else (
          match get_field_from_object !obj identifier with
          | None ->
            M.fail
              (UnknownField ("FIXME: тут должно быть имя класса объекта", identifier))
          | Some rc ->
            let field_content =
              match rc.content with
              | Variable v -> v
              | _ -> failwith "should not reach here"
            in
            M.return
              { ctx with
                last_eval_expr = !(field_content.value)
              ; last_derefered_variable = Some field_content
              })
      | _ ->
        (* TODO: необходимо изменить принцип хранения переменных, методов и классов. Их нужно хранить раздельно *)
        (match get_var_from_env ctx.environment identifier with
        | None -> M.fail (UnknownVariable identifier)
        | Some rc ->
          (match rc.content with
          | Variable var ->
            M.return
              { ctx with
                last_eval_expr = !(var.value)
              ; last_derefered_variable = Some var
              }
          | _ -> failwith "should not reach here")))
    | Dereference (obj_expression, der_expression) ->
      interpret_expression ctx obj_expression
      >>= fun eval_ctx ->
      (match eval_ctx.last_eval_expr with
      | Object obj ->
        let obj_ctx = { ctx with scope = Object (ref obj) } in
        interpret_expression obj_ctx der_expression
      | _ -> M.fail ExprectedObjectToDereference)
    | Add (l, r) ->
      eval_bin_args l r
      >>= (function
      | IntValue x, IntValue y -> M.return { ctx with last_eval_expr = IntValue (x + y) }
      | _ -> failwith "not inplemented expression type in Add")
    | Mul (l, r) ->
      eval_bin_args l r
      >>= (function
      | IntValue x, IntValue y -> M.return { ctx with last_eval_expr = IntValue (x * y) }
      | _ -> failwith "not inplemented expression type in Mul")
    | Sub (l, r) ->
      eval_bin_args l r
      >>= (function
      | IntValue x, IntValue y -> M.return { ctx with last_eval_expr = IntValue (x - y) }
      | _ -> failwith "not inplemented expression type in Sub")
    | And (l, r) ->
      eval_bin_args l r
      >>= (function
      | BooleanValue x, BooleanValue y ->
        M.return { ctx with last_eval_expr = BooleanValue (x && y) }
      | _ -> failwith "not inplemented expression type in And")
    | Or (l, r) ->
      eval_bin_args l r
      >>= (function
      | BooleanValue x, BooleanValue y ->
        M.return { ctx with last_eval_expr = BooleanValue (x || y) }
      | _ -> failwith "not inplemented expression type in Or")
    | Not x ->
      eval_un_arg x
      >>= (function
      | BooleanValue x -> M.return { ctx with last_eval_expr = BooleanValue (not x) }
      | _ -> failwith "not inplemented expression type in Not")
    | Equal (l, r) ->
      eval_bin_args l r
      >>= (function
      | IntValue x, IntValue y ->
        M.return { ctx with last_eval_expr = BooleanValue (x == y) }
      | _ -> failwith "not inplemented expression type in Equal")
    | Less (l, r) ->
      eval_bin_args l r
      >>= (function
      | IntValue x, IntValue y ->
        M.return { ctx with last_eval_expr = BooleanValue (x < y) }
      | _ -> failwith "not inplemented expression type in Less")
    | _ -> failwith "not inplemented expression type"
  ;;
end

let parse_and_run input =
  let open Opal in
  let open Statement in
  let open Interpret (Result) in
  let ctx =
    { environment = []
    ; last_eval_expr = Unitialized
    ; last_return_value = Unitialized
    ; last_derefered_variable = None
    ; scope = Initialize
    }
  in
  match apply_parser statement input with
  | None -> Error EmptyProgram
  | Some statement -> run ctx statement
;;
