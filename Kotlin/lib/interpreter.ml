open Base
open Utils
open Parser
open Ast

module InterpreterResult = struct
  include Base.Result

  let ( let* ) x f = x >>= f
end

module Interpret = struct
  open InterpreterResult

  type scope_type =
    | Initialize
    | Function
    | ObjectInitialization of object_t ref
    | DereferencedObject of object_t * context
    | ThisDereferencedObject of object_t * context
    | Method of object_t
    | AnonymousFunction of object_t option
  [@@deriving show]

  and context =
    { environment : record_t list
    ; not_nullable_records : record_t list
    ; last_eval_expression : value
          (** Данная переменная используется для сохранения значения последленного исполненого Ast.expression (по умолчанию Unitialized) *)
    ; last_return_value : value
          (** Данная переменная используется для сохранения значения последнего исполненого return (по умолчанию Unitialized) *)
    ; last_derefered_variable : (record_t * variable_t) option
          (** Данная переменная используется для сохранения последней переменной, к которой обратились через VarIdentifier (по умолчанию None) *)
    ; scope : scope_type
    }

  let empty_ctx =
    { environment = []
    ; not_nullable_records = []
    ; last_eval_expression = Unitialized None
    ; last_return_value = Unitialized None
    ; last_derefered_variable = None
    ; scope = Initialize
    }
  ;;

  let check_typename_value_correspondance = function
    | Dynamic, _ -> true
    | FunctionType _, Ast.AnonymousFunction _ -> true
    | (Int | Nullable Int), IntValue _ -> true
    | (String | Nullable String), StringValue _ -> true
    | (Boolean | Nullable Boolean), BooleanValue _ -> true
    | Nullable _, NullValue -> true
    | Unit, Unitialized _ -> true
    | (ClassIdentifier identifier | Nullable (ClassIdentifier identifier)), Object obj
      when String.equal obj.obj_class.classname identifier -> true
    | _, _ -> false
  ;;

  let check_record_contains_modifier (rc : record_t) modifier =
    Option.is_some
      (List.find rc.modifiers ~f:(function
          | found_modifier when Poly.( = ) found_modifier modifier -> true
          | _ -> false))
  ;;

  let check_record_is_private rc = check_record_contains_modifier rc Private
  let check_record_is_protected rc = check_record_contains_modifier rc Protected
  let check_record_is_open rc = check_record_contains_modifier rc Open
  let check_record_is_override rc = check_record_contains_modifier rc Override

  let get_this_from_ctx ctx =
    match ctx.scope with
    | Method obj | ObjectInitialization { contents = obj } | AnonymousFunction (Some obj)
      -> return obj
    | _ -> fail ThisExpressionError
  ;;

  let get_var_from_env env name =
    List.find_map env ~f:(fun r ->
        match r.name, r.content with
        | r_name, Variable content when String.equal r_name name -> Some (r, content)
        | _ -> None)
  ;;

  let get_function_from_env env name =
    List.find_map env ~f:(fun r ->
        match r.name, r.content with
        | r_name, Function content when String.equal r_name name -> Some (r, content)
        | _ -> None)
  ;;

  let get_class_from_env env name =
    List.find_map env ~f:(fun r ->
        match r.name, r.content with
        | r_name, Class content when String.equal r_name name -> Some (r, content)
        | _ -> None)
  ;;

  let define_in_ctx ctx rc =
    match rc.content with
    | Variable _ ->
      let var_names =
        List.filter_map ctx.environment ~f:(function rc ->
            (match rc.content with
            | Variable _ -> Some rc.name
            | _ -> None))
      in
      if not (List.mem var_names rc.name ~equal:String.equal)
      then (
        let new_environment = rc :: ctx.environment in
        rc.clojure := rc :: !(rc.clojure);
        Some { ctx with environment = new_environment })
      else None
    | Function _ ->
      let func_names =
        List.filter_map ctx.environment ~f:(function rc ->
            (match rc.content with
            | Function _ -> Some rc.name
            | _ -> None))
      in
      if not (List.mem func_names rc.name ~equal:String.equal)
      then (
        let new_environment = rc :: ctx.environment in
        rc.clojure := rc :: !(rc.clojure);
        Some { ctx with environment = new_environment })
      else None
    | Class _ ->
      let cls_names =
        List.filter_map ctx.environment ~f:(function rc ->
            (match rc.content with
            | Class _ -> Some rc.name
            | _ -> None))
      in
      if not (List.mem cls_names rc.name ~equal:String.equal)
      then (
        let new_environment = rc :: ctx.environment in
        rc.clojure := rc :: !(rc.clojure);
        Some { ctx with environment = new_environment })
      else None
  ;;

  let rec get_from_object finder this_flag obj name =
    let open Option.Let_syntax in
    match finder obj with
    | Some (field, content)
      when ((check_record_is_private field || check_record_is_protected field)
           && this_flag)
           || not (check_record_is_private field && check_record_is_protected field) ->
      Some (field, content)
    | None ->
      obj.super
      >>= fun super ->
      get_from_object finder true super name
      >>= fun (field, content) ->
      if check_record_is_private field
         || (check_record_is_protected field && not this_flag)
      then None
      else Some (field, content)
    | _ -> None
  ;;

  let get_field_from_object this_flag obj name =
    get_from_object
      (fun obj ->
        List.find_map obj.fields ~f:(fun r ->
            match r.name, r.content with
            | r_name, Variable content when String.equal r_name name -> Some (r, content)
            | _ -> None))
      this_flag
      obj
      name
  ;;

  let get_method_from_object this_flag obj name =
    get_from_object
      (fun obj ->
        List.find_map obj.methods ~f:(fun r ->
            match r.name, r.content with
            | r_name, Function content when String.equal r_name name -> Some (r, content)
            | _ -> None))
      this_flag
      obj
      name
  ;;

  let define_in_object obj rc =
    let rec iter_from_obj_to_super (cur : object_t option) func =
      match cur with
      | None -> []
      | Some o -> func o @ iter_from_obj_to_super o.super func
    in
    rc.clojure := rc :: !(rc.clojure);
    match rc.content with
    | Variable _ ->
      let defined_fields = iter_from_obj_to_super (Some obj) (fun o -> o.fields) in
      let defined_field_with_rc_name =
        List.find defined_fields ~f:(fun r -> String.equal r.name rc.name)
      in
      if Option.is_none defined_field_with_rc_name
      then (
        let new_fields = rc :: obj.fields in
        Some { obj with fields = new_fields })
      else (
        let declared_field = Option.value_exn defined_field_with_rc_name in
        if check_record_is_open declared_field && check_record_is_override rc
        then (
          let open_rc = { rc with modifiers = Open :: rc.modifiers } in
          let new_fields = open_rc :: obj.fields in
          Some { obj with fields = new_fields })
        else None)
    | Function _ ->
      let defined_methods = iter_from_obj_to_super (Some obj) (fun o -> o.methods) in
      let defined_method_with_rc_name =
        List.find defined_methods ~f:(fun r -> String.equal r.name rc.name)
      in
      if Option.is_none defined_method_with_rc_name
      then (
        let new_methods = rc :: obj.methods in
        Some { obj with methods = new_methods })
      else (
        let declared_method = Option.value_exn defined_method_with_rc_name in
        if check_record_is_open declared_method && check_record_is_override rc
        then (
          let open_rc = { rc with modifiers = Open :: rc.modifiers } in
          let new_methods = open_rc :: obj.methods in
          Some { obj with methods = new_methods })
        else None)
    | Class _ -> failwith "Not supported"
  ;;

  let rec get_enclosing_object ctx =
    match ctx.scope with
    | Method obj | ObjectInitialization { contents = obj } | AnonymousFunction (Some obj)
      -> Some obj
    | DereferencedObject (_, outer_ctx) | ThisDereferencedObject (_, outer_ctx) ->
      get_enclosing_object outer_ctx
    | _ -> None
  ;;

  let last_identity = ref 0

  let get_unique_identity_code () =
    let code = !last_identity in
    last_identity := code + 1;
    code
  ;;

  let get_var_from_ctx ctx identifier =
    let get_var_from_dereference_helper obj this_flag =
      match get_field_from_object this_flag obj identifier with
      | None -> fail (UnknownField (obj.obj_class.classname, identifier))
      | Some (rc, field_content) -> return (rc, field_content)
    in
    let get_var_from_environment_helper () =
      match get_var_from_env ctx.environment identifier with
      | None -> fail (UnknownVariable identifier)
      | Some (rc, var) -> return (rc, var)
    in
    match ctx.scope with
    | ObjectInitialization { contents = obj } | Method obj | AnonymousFunction (Some obj)
      ->
      (match get_var_from_dereference_helper obj true with
      | Ok ctx -> return ctx
      | Error _ -> get_var_from_environment_helper ())
    | DereferencedObject (obj, _) | ThisDereferencedObject (obj, _) ->
      let this_flag =
        match ctx.scope with
        | ThisDereferencedObject _ -> true
        | _ -> false
      in
      get_var_from_dereference_helper obj this_flag
    | _ -> get_var_from_environment_helper ()
  ;;

  let get_function_from_ctx ctx identifier =
    let get_function_from_dereference_helper obj this_flag =
      match get_method_from_object this_flag obj identifier with
      | None ->
        (match get_field_from_object this_flag obj identifier with
        | None -> fail (UnknownMethod (obj.obj_class.classname, identifier))
        | Some (rc, content) ->
          (match !(content.value) with
          | AnonymousFunction f -> return f
          | _ -> fail (UnknownFunction identifier))
          >>= fun func -> return (`AnonymousFunction (rc, func)))
      | Some (meth, meth_content) -> return (`Method (obj, meth, meth_content))
    in
    let get_function_from_environment_helper () =
      match get_class_from_env ctx.environment identifier with
      | None ->
        (match get_function_from_env ctx.environment identifier with
        | Some (rc, func) -> return (`Function (rc, func))
        | None ->
          (match get_var_from_env ctx.environment identifier with
          | None -> fail (UnknownFunction identifier)
          | Some (rc, content) ->
            (match !(content.value) with
            | AnonymousFunction f -> return f
            | _ -> fail (UnknownFunction identifier))
            >>= fun func -> return (`AnonymousFunction (rc, func))))
      | Some (rc, class_data) -> return (`Class (rc, class_data))
    in
    match ctx.scope with
    | ObjectInitialization { contents = obj } | Method obj | AnonymousFunction (Some obj)
      ->
      (match get_function_from_dereference_helper obj true with
      | Ok ctx -> return ctx
      | Error _ -> get_function_from_environment_helper ())
    | DereferencedObject (obj, _) | ThisDereferencedObject (obj, _) ->
      let this_flag =
        match ctx.scope with
        | ThisDereferencedObject _ -> true
        | _ -> false
      in
      get_function_from_dereference_helper obj this_flag
    | _ -> get_function_from_environment_helper ()
  ;;

  (* TODO: поменять ошибки DereferenceError на что нибудь более осмысленное *)
  let rec check_expression_is_nullable ctx =
    let check_dereference_nullable obj_expression der_expression =
      check_expression_is_nullable ctx obj_expression
      >>= function
      | true -> return true
      | _ ->
        let get_class_or_primitive_typename_of_method cur_class identifier =
          match
            List.find cur_class.method_initializers ~f:(fun meth ->
                if String.equal meth.identifier identifier then true else false)
          with
          | None -> fail (UnknownMethod (cur_class.classname, identifier))
          | Some meth ->
            (match meth.fun_typename with
            | Nullable _ | Dynamic -> return `Nullable
            | ClassIdentifier identifier ->
              (match get_class_from_env ctx.environment identifier with
              | None -> fail (UnknownClass identifier)
              | Some (_, class_data) -> return (`Class class_data))
            | other -> return (`Primitive other))
        in
        let get_class_or_primitive_typename_of_field cur_class identifier =
          match
            List.find cur_class.field_initializers ~f:(fun field ->
                if String.equal field.identifier identifier then true else false)
          with
          | None -> fail (UnknownField (cur_class.classname, identifier))
          | Some field ->
            (match field.var_typename with
            | Nullable _ | Dynamic -> return `Nullable
            | ClassIdentifier identifier ->
              (match get_class_from_env ctx.environment identifier with
              | None -> fail (UnknownClass identifier)
              | Some (_, class_data) -> return (`Class class_data))
            | other -> return (`Primitive other))
        in
        let rec check_dereference_nullable_helper cur_class = function
          | VarIdentifier identifier ->
            get_class_or_primitive_typename_of_field cur_class identifier
            >>= (function
            | `Nullable -> return true
            | _ -> return false)
          | FunctionCall (identifier, _) ->
            get_class_or_primitive_typename_of_method cur_class identifier
            >>= (function
            | `Nullable -> return true
            | _ -> return false)
          | Dereference (VarIdentifier identifier, der_expression)
          | ElvisDereference (VarIdentifier identifier, der_expression) ->
            get_class_or_primitive_typename_of_field cur_class identifier
            >>= (function
            | `Nullable -> return true
            | `Class class_data ->
              check_dereference_nullable_helper class_data der_expression
            | `Primitive _ ->
              failwith "Not implemented. Expected field of non-primitive class type")
          | Dereference (FunctionCall (identifier, _), der_expression)
          | ElvisDereference (FunctionCall (identifier, _), der_expression) ->
            get_class_or_primitive_typename_of_method cur_class identifier
            >>= (function
            | `Nullable -> return true
            | `Class class_data ->
              check_dereference_nullable_helper class_data der_expression
            | `Primitive _ ->
              failwith "Not implemented. Expected method of non-primitive class type")
          | _ -> fail DereferenceError
        in
        (* На данный момент поддерживаются только Dereference(obj_expression, der_expression) такого вида, что obj_expression = VarIdentifier | FunctionCall | This, причем VarIdentifier | FunctionCall должны производить объект класса не примитивного типа *)
        (match obj_expression with
        | VarIdentifier identifier ->
          let* _, var = get_var_from_ctx ctx identifier in
          (match !(var.value) with
          | Object obj -> check_dereference_nullable_helper obj.obj_class der_expression
          | _ -> failwith "Not implemented. Expected variable of non-primitive class type")
        | FunctionCall (identifier, _) ->
          get_function_from_ctx ctx identifier
          >>= (function
          | `AnonymousFunction (_, func) | `Function (_, func) | `Method (_, _, func) ->
            (match func.fun_typename with
            | Nullable _ -> return true
            | ClassIdentifier identifier ->
              (match get_class_from_env ctx.environment identifier with
              | None -> fail (UnknownClass identifier)
              | Some (_, class_data) ->
                check_dereference_nullable_helper class_data der_expression)
            | _ ->
              failwith "Not implemented. Expected function of non-primitive class type")
          | `Class (_, class_data) ->
            check_dereference_nullable_helper class_data der_expression)
        | This ->
          let* this = get_this_from_ctx ctx in
          check_dereference_nullable_helper this.obj_class der_expression
        | _ -> fail DereferenceError)
    in
    function
    | VarIdentifier identifier ->
      let* rc, var = get_var_from_ctx ctx identifier in
      if List.mem ctx.not_nullable_records rc ~equal:phys_equal
      then return true
      else (
        match var.var_typename with
        | Nullable _ -> return true
        | _ -> return false)
    | FunctionCall (identifier, _) ->
      get_function_from_ctx ctx identifier
      >>= (function
      | `AnonymousFunction (_, func) | `Function (_, func) | `Method (_, _, func) ->
        (match func.fun_typename with
        | Nullable _ -> return true
        | _ -> return false)
      | `Class _ -> return false)
    | Dereference (obj_expression, der_expression)
    | ElvisDereference (obj_expression, der_expression) ->
      check_dereference_nullable obj_expression der_expression
    | Const NullValue -> return true
    | _ -> return false
  ;;

  let rec interpret_statement ctx stat =
    match stat with
    | InitInClass statement_block -> interpret_statement ctx statement_block
    | Block statements ->
      let exception Return_exception of context in
      (try
         List.fold statements ~init:(return ctx) ~f:(fun monadic_ctx inner_stat ->
             monadic_ctx
             >>= fun checked_ctx ->
             interpret_statement checked_ctx inner_stat
             >>= fun eval_ctx ->
             match inner_stat with
             | Return _ -> raise (Return_exception eval_ctx)
             | If _ -> return eval_ctx
             | _ -> return eval_ctx)
         >>= fun ret_ctx ->
         match ctx.scope with
         | AnonymousFunction _ ->
           raise
             (Return_exception
                { ret_ctx with last_return_value = ret_ctx.last_eval_expression })
         | _ -> return ret_ctx
       with
      | Return_exception returned_ctx ->
        (match ctx.scope with
        | Method _ | AnonymousFunction _ | Function -> return returned_ctx
        | _ -> fail ReturnNotInFunction))
    | InitializeBlock statements ->
      List.fold statements ~init:(return ctx) ~f:(fun monadic_ctx stat ->
          monadic_ctx >>= fun checked_ctx -> interpret_statement checked_ctx stat)
    | Return expression ->
      interpret_expression ctx expression
      >>= fun ret_ctx ->
      return { ctx with last_return_value = ret_ctx.last_eval_expression }
    | Expression expression -> interpret_expression ctx expression
    | ClassDeclaration (modifiers, name, constructor_args, super_call_option, statements)
      ->
      let* super_constructor =
        match super_call_option with
        | None when not (String.equal name "Any") ->
          (* Option.value_exn здесь не должен кидать исключений, так как предполагается, что класс Any обязательно должен быть объявлен *)
          let _, any_class =
            Option.value_exn (get_class_from_env ctx.environment "Any")
          in
          return (Some (any_class, FunctionCall ("Any", [])))
        | Some (class_identifier, arg_expressions) ->
          (match get_class_from_env ctx.environment class_identifier with
          | None -> fail (UnknownClass class_identifier)
          | Some (_, found_class) ->
            return (Some (found_class, FunctionCall (class_identifier, arg_expressions))))
        | _ -> return None
      in
      let field_initializers, method_initializers, init_statements =
        List.fold_right
          statements
          ~f:(fun stat (field_initializers, method_initializers, init_statements) ->
            match stat with
            | VarDeclaration var_initializer ->
              var_initializer :: field_initializers, method_initializers, init_statements
            | FunDeclaration fun_initializer ->
              field_initializers, fun_initializer :: method_initializers, init_statements
            | InitInClass block ->
              field_initializers, method_initializers, block :: init_statements
            | _ ->
              failwith
                ("should not reach here. Parser must not parse with statement inside \
                  class: "
                ^ show_statement stat))
          ~init:([], [], [])
      in
      (match
         define_in_ctx
           ctx
           { name
           ; modifiers
           ; clojure = ref ctx.environment
           ; enclosing_object = ref (get_enclosing_object ctx)
           ; content =
               Class
                 { classname = name
                 ; constructor_args
                 ; super_constructor
                 ; field_initializers
                 ; method_initializers
                 ; init_statements
                 }
           }
       with
      | None -> fail (Redeclaration name)
      | Some new_ctx -> return new_ctx)
    | FunDeclaration
        { modifiers
        ; identifier = name
        ; args = arguments
        ; fun_typename
        ; fun_statement = statement
        } ->
      (match
         define_in_ctx
           ctx
           { name
           ; modifiers
           ; clojure = ref ctx.environment
           ; enclosing_object = ref (get_enclosing_object ctx)
           ; content =
               Function
                 { identity_code = get_unique_identity_code ()
                 ; fun_typename
                 ; arguments
                 ; statement
                 }
           }
       with
      | None -> fail (Redeclaration name)
      | Some new_ctx -> return new_ctx)
    | VarDeclaration
        { modifiers; var_modifier; identifier = name; var_typename; init_expression } ->
      (match init_expression with
      | None -> return (Unitialized None)
      | Some expr ->
        interpret_expression ctx expr
        >>= fun interpreted_ctx -> return interpreted_ctx.last_eval_expression)
      >>= fun value ->
      (match
         define_in_ctx
           ctx
           { name
           ; modifiers
           ; clojure = ref ctx.environment
           ; enclosing_object = ref (get_enclosing_object ctx)
           ; content =
               Variable
                 { var_typename
                 ; mutable_status = Poly.( = ) var_modifier Var
                 ; value = ref value
                 }
           }
       with
      | None -> fail (Redeclaration name)
      | Some new_ctx -> return new_ctx)
    | Assign (var_identifier_expression, assign_expression) ->
      interpret_expression ctx var_identifier_expression
      >>= fun identifier_ctx ->
      (match identifier_ctx.last_derefered_variable with
      | None -> fail ExpectedVarIdentifer
      | Some (rc, var) ->
        interpret_expression ctx assign_expression
        >>= fun assign_value_ctx ->
        (match
           ( check_typename_value_correspondance
               (var.var_typename, assign_value_ctx.last_eval_expression)
           , ctx.scope
           , !(var.value) )
         with
        | true, ObjectInitialization obj, Unitialized (Some obj_of_field)
          when phys_equal obj obj_of_field ->
          rc.clojure := identifier_ctx.environment;
          rc.enclosing_object := get_enclosing_object ctx;
          var.value := assign_value_ctx.last_eval_expression;
          return assign_value_ctx
        | true, _, _ when var.mutable_status ->
          rc.clojure := identifier_ctx.environment;
          rc.enclosing_object := get_enclosing_object ctx;
          var.value := assign_value_ctx.last_eval_expression;
          return assign_value_ctx
        | true, _, _ -> fail (VariableNotMutable rc.name)
        | false, _, _ ->
          fail
            (VariableValueTypeMismatch
               (rc.name, var.var_typename, assign_value_ctx.last_eval_expression))))
    | If (log_expr, if_statement, else_statement) ->
      interpret_expression ctx log_expr
      >>= fun eval_ctx ->
      (match eval_ctx.last_eval_expression with
      | BooleanValue log_val when log_val -> interpret_statement ctx if_statement
      | BooleanValue log_val when not log_val ->
        (match else_statement with
        | None -> return ctx
        | Some stat -> interpret_statement ctx stat)
      | _ -> fail ExpectedBooleanValue)
    | While (log_expr, while_statement) ->
      interpret_expression ctx log_expr
      >>= fun eval_ctx ->
      (match eval_ctx.last_eval_expression with
      | BooleanValue log_val when log_val ->
        interpret_statement ctx while_statement >>= fun _ -> interpret_statement ctx stat
      | BooleanValue log_val when not log_val -> return eval_ctx
      | _ -> fail ExpectedBooleanValue)

  and interpret_expression ctx expr =
    let eval_bin_args l r =
      interpret_expression ctx l
      >>= fun l_ctx ->
      interpret_expression ctx r
      >>= fun r_ctx -> return (l_ctx.last_eval_expression, r_ctx.last_eval_expression)
    in
    let eval_un_arg x =
      interpret_expression ctx x >>= fun x_ctx -> return x_ctx.last_eval_expression
    in
    match expr with
    | Println print_expr ->
      interpret_expression ctx print_expr
      >>= fun eval_ctx ->
      (match eval_ctx.last_eval_expression with
      | IntValue v -> Stdio.print_endline (Int.to_string v)
      | StringValue v -> Stdio.print_endline v
      | BooleanValue v -> Stdio.print_endline (if v then "true" else "false")
      | NullValue | Unitialized _ -> Stdio.print_endline "null"
      | Object obj ->
        Stdlib.Printf.printf "%s@%x\n" obj.obj_class.classname obj.identity_code
      | AnonymousFunction func ->
        let args_count = List.length func.arguments in
        let args_string =
          List.fold func.arguments ~init:"" ~f:(fun acc (_, arg_typename) ->
              Stdlib.Printf.sprintf "%s%s, " acc (show_typename arg_typename))
        in
        Stdlib.Printf.printf
          "Function%d<%s%s>\n"
          args_count
          args_string
          (show_typename func.fun_typename));
      return { ctx with last_eval_expression = NullValue }
    | AnonymousFunctionDeclaration (arguments, statement) ->
      let func =
        { identity_code = get_unique_identity_code ()
        ; fun_typename = Dynamic
        ; arguments
        ; statement
        }
      in
      return { ctx with last_eval_expression = AnonymousFunction func }
    | FunctionCall (identifier, arg_expressions) ->
      let define_args define_ctx args exprs =
        let args_ctx =
          match ctx.scope with
          | DereferencedObject (_, outer_ctx) | ThisDereferencedObject (_, outer_ctx) ->
            outer_ctx
          | _ -> ctx
        in
        List.fold2
          args
          exprs
          ~init:(return define_ctx)
          ~f:(fun monadic_ctx (arg_name, arg_typename) arg_expr ->
            monadic_ctx
            >>= fun monadic_ctx ->
            interpret_expression args_ctx arg_expr
            >>= fun eval_expr_ctx ->
            if check_typename_value_correspondance
                 (arg_typename, eval_expr_ctx.last_eval_expression)
            then (
              match
                define_in_ctx
                  monadic_ctx
                  { name = arg_name
                  ; modifiers = []
                  ; clojure = ref args_ctx.environment
                  ; enclosing_object = ref (get_enclosing_object args_ctx)
                  ; content =
                      Variable
                        { var_typename = arg_typename
                        ; mutable_status = false
                        ; value = ref eval_expr_ctx.last_eval_expression
                        }
                  }
              with
              | None -> fail (Redeclaration arg_name)
              | Some defined_ctx -> return defined_ctx)
            else fail (VariableTypeMismatch arg_name))
      in
      let eval_function new_ctx func =
        match define_args new_ctx func.arguments arg_expressions with
        | Ok fun_ctx ->
          fun_ctx
          >>= fun fun_ctx ->
          interpret_statement
            { fun_ctx with last_return_value = Unitialized None }
            func.statement
          >>= fun func_eval_ctx ->
          if check_typename_value_correspondance
               (func.fun_typename, func_eval_ctx.last_return_value)
          then
            if (not (Poly.( = ) func.fun_typename Unit))
               && Poly.( = ) func_eval_ctx.last_return_value (Unitialized None)
            then fail (ExpectedReturnInFunction identifier)
            else
              return
                { ctx with
                  last_eval_expression = func_eval_ctx.last_return_value
                ; last_return_value = Unitialized None
                }
          else
            fail
              (FunctionReturnTypeMismatch
                 (identifier, func.fun_typename, func_eval_ctx.last_return_value))
        | _ -> fail FunctionArgumentsCountMismatch
      in
      get_function_from_ctx ctx identifier
      >>= (function
      | `AnonymousFunction (rc, func) ->
        let new_ctx =
          { ctx with
            environment = !(rc.clojure)
          ; scope = AnonymousFunction !(rc.enclosing_object)
          }
        in
        eval_function new_ctx func
      | `Class (rc, class_data) ->
        let new_object =
          ref
            { identity_code = get_unique_identity_code ()
            ; super = None
            ; obj_class = class_data
            ; fields = []
            ; methods = []
            }
        in
        let new_ctx = { ctx with environment = !(rc.clojure); scope = Function } in
        (match define_args new_ctx class_data.constructor_args arg_expressions with
        | Ok constructor_ctx ->
          constructor_ctx
          >>= fun class_ctx ->
          (match class_data.super_constructor with
          | None -> return class_ctx
          | Some (_, expr) ->
            (match expr with
            | FunctionCall (identifier, _) ->
              (match get_class_from_env ctx.environment identifier with
              | Some (rc, _) when check_record_is_open rc -> return expr
              | Some _ -> fail (ClassNotOpen identifier)
              | None -> fail (UnknownFunction identifier))
            | _ -> fail ClassSuperConstructorNotValid)
            >>= fun _ ->
            interpret_expression class_ctx expr
            >>= fun sup_ctx ->
            return sup_ctx.last_eval_expression
            >>= (function
            | Object super_object ->
              new_object := { !new_object with super = Some super_object };
              return class_ctx
            | _ -> fail ClassSuperConstructorNotValid))
          >>= fun evaluated_constructor_ctx ->
          return
            { evaluated_constructor_ctx with scope = ObjectInitialization new_object }
          >>= fun class_inner_ctx ->
          let* class_with_fields_inner_ctx =
            List.fold
              class_data.field_initializers
              ~init:(return class_inner_ctx)
              ~f:(fun
                   monadic_ctx
                   { modifiers
                   ; var_modifier
                   ; identifier = name
                   ; var_typename
                   ; init_expression
                   }
                 ->
                let* checked_ctx = monadic_ctx in
                let* init_value =
                  match init_expression with
                  | None -> return (Unitialized (Some new_object))
                  | Some expr ->
                    let* eval_ctx = interpret_expression class_inner_ctx expr in
                    return eval_ctx.last_eval_expression
                in
                match
                  define_in_object
                    !new_object
                    { name
                    ; modifiers
                    ; clojure = ref ctx.environment
                    ; enclosing_object = ref (get_enclosing_object ctx)
                    ; content =
                        Variable
                          { var_typename
                          ; mutable_status =
                              (match var_modifier with
                              | Var -> true
                              | Val -> false)
                          ; value = ref init_value
                          }
                    }
                with
                | None -> fail (Redeclaration name)
                | Some updated_object ->
                  new_object := updated_object;
                  return checked_ctx)
          in
          let* class_with_methods_inner_ctx =
            List.fold
              class_data.method_initializers
              ~init:(return class_with_fields_inner_ctx)
              ~f:(fun
                   monadic_ctx
                   { modifiers
                   ; identifier = name
                   ; args = arguments
                   ; fun_typename
                   ; fun_statement = statement
                   }
                 ->
                let* checked_ctx = monadic_ctx in
                match
                  define_in_object
                    !new_object
                    { name
                    ; modifiers
                    ; clojure = ref ctx.environment
                    ; enclosing_object = ref (get_enclosing_object ctx)
                    ; content =
                        Function
                          { identity_code = get_unique_identity_code ()
                          ; fun_typename
                          ; arguments
                          ; statement
                          }
                    }
                with
                | None -> fail (Redeclaration name)
                | Some updated_object ->
                  new_object := updated_object;
                  return checked_ctx)
          in
          List.fold
            class_data.init_statements
            ~init:(return class_with_methods_inner_ctx)
            ~f:(fun monadic_ctx stat ->
              let* checked_ctx = monadic_ctx in
              interpret_statement checked_ctx stat)
          >>= fun _ -> return { ctx with last_eval_expression = Object !new_object }
        | _ -> fail FunctionArgumentsCountMismatch)
      | `Function (rc, func) ->
        let new_ctx = { ctx with environment = !(rc.clojure); scope = Function } in
        eval_function new_ctx func
      | `Method (obj, rc, meth) ->
        let new_ctx = { ctx with environment = !(rc.clojure); scope = Method obj } in
        eval_function new_ctx meth)
    | Const x -> return { ctx with last_eval_expression = x }
    | This -> fail ThisExpressionError
    | VarIdentifier identifier ->
      let* rc, var = get_var_from_ctx ctx identifier in
      return
        { ctx with
          last_eval_expression = !(var.value)
        ; last_derefered_variable = Some (rc, var)
        }
    | Dereference (obj_expression, der_expression)
    | ElvisDereference (obj_expression, der_expression) ->
      check_expression_is_nullable ctx obj_expression
      >>= fun nullable_flag ->
      (match expr with
      | Dereference _ when nullable_flag ->
        (* TODO: сделать обработку данного случая *)
        return ""
      | _ -> return "")
      >>= fun _ ->
      (match obj_expression with
      | This ->
        let* this = get_this_from_ctx ctx in
        return ({ ctx with last_eval_expression = Object this }, true)
      | _ ->
        interpret_expression ctx obj_expression
        >>= fun obj_expr_eval_ctx -> return (obj_expr_eval_ctx, false))
      >>= fun (eval_ctx, this_flag) ->
      (match eval_ctx.last_eval_expression with
      | Object obj ->
        let scope =
          match ctx.scope with
          | (DereferencedObject (_, outer_ctx) | ThisDereferencedObject (_, outer_ctx))
            when this_flag -> ThisDereferencedObject (obj, outer_ctx)
          | DereferencedObject (_, outer_ctx) | ThisDereferencedObject (_, outer_ctx) ->
            DereferencedObject (obj, outer_ctx)
          | _ when this_flag -> ThisDereferencedObject (obj, ctx)
          | _ -> DereferencedObject (obj, ctx)
        in
        let obj_ctx = { ctx with scope } in
        (match der_expression with
        | VarIdentifier _ | FunctionCall _ | Dereference _ | ElvisDereference _ ->
          interpret_expression obj_ctx der_expression
          >>= fun ctx_with_eval_dereference ->
          return { ctx_with_eval_dereference with scope = ctx.scope }
        | _ -> fail DereferenceError)
      | NullValue | Unitialized _ ->
        (match expr with
        | ElvisDereference _ -> return { ctx with last_eval_expression = NullValue }
        | _ -> fail ExpectedObjectToDereference)
      | _ -> fail ExpectedObjectToDereference)
    | Add (l, r) ->
      eval_bin_args l r
      >>= (function
      | IntValue x, IntValue y ->
        return { ctx with last_eval_expression = IntValue (x + y) }
      | StringValue x, StringValue y ->
        return { ctx with last_eval_expression = StringValue (x ^ y) }
      | _ -> fail (UnsupportedOperandTypes expr))
    | Mul (l, r) ->
      eval_bin_args l r
      >>= (function
      | IntValue x, IntValue y ->
        return { ctx with last_eval_expression = IntValue (x * y) }
      | _ -> fail (UnsupportedOperandTypes expr))
    | Sub (l, r) ->
      eval_bin_args l r
      >>= (function
      | IntValue x, IntValue y ->
        return { ctx with last_eval_expression = IntValue (x - y) }
      | _ -> fail (UnsupportedOperandTypes expr))
    | And (l, r) ->
      eval_bin_args l r
      >>= (function
      | BooleanValue x, BooleanValue y ->
        return { ctx with last_eval_expression = BooleanValue (x && y) }
      | _ -> fail (UnsupportedOperandTypes expr))
    | Or (l, r) ->
      eval_bin_args l r
      >>= (function
      | BooleanValue x, BooleanValue y ->
        return { ctx with last_eval_expression = BooleanValue (x || y) }
      | _ -> fail (UnsupportedOperandTypes expr))
    | Not x ->
      eval_un_arg x
      >>= (function
      | BooleanValue x -> return { ctx with last_eval_expression = BooleanValue (not x) }
      | _ -> fail (UnsupportedOperandTypes expr))
    | Equal (l, r) ->
      eval_bin_args l r
      >>= (function
      | IntValue x, IntValue y ->
        return { ctx with last_eval_expression = BooleanValue (x = y) }
      | StringValue x, StringValue y ->
        return { ctx with last_eval_expression = BooleanValue (String.equal x y) }
      | BooleanValue x, BooleanValue y ->
        return
          { ctx with
            last_eval_expression = BooleanValue ((x && y) || ((not x) && not y))
          }
      | AnonymousFunction x, AnonymousFunction y ->
        return
          { ctx with
            last_eval_expression = BooleanValue (x.identity_code = y.identity_code)
          }
      | Object x, Object y ->
        return
          { ctx with
            last_eval_expression = BooleanValue (x.identity_code = y.identity_code)
          }
      | NullValue, NullValue
      | NullValue, Unitialized _
      | Unitialized _, NullValue
      | Unitialized _, Unitialized _ ->
        return { ctx with last_eval_expression = BooleanValue true }
      | NullValue, _ | _, NullValue ->
        return { ctx with last_eval_expression = BooleanValue false }
      | _ -> return { ctx with last_eval_expression = BooleanValue false })
    | Less (l, r) ->
      eval_bin_args l r
      >>= (function
      | IntValue x, IntValue y ->
        return { ctx with last_eval_expression = BooleanValue (x < y) }
      | _ -> fail (UnsupportedOperandTypes expr))
    | Div (l, r) ->
      eval_bin_args l r
      >>= (function
      | IntValue x, IntValue y ->
        return { ctx with last_eval_expression = IntValue (x / y) }
      | _ -> fail (UnsupportedOperandTypes expr))
    | Mod (l, r) ->
      eval_bin_args l r
      >>= (function
      | IntValue x, IntValue y ->
        return { ctx with last_eval_expression = IntValue (x % y) }
      | _ -> fail (UnsupportedOperandTypes expr))
  ;;

  let load_standard_classes ctx =
    List.fold Std.standard_classes ~init:(return ctx) ~f:(fun monadic_ctx class_string ->
        monadic_ctx
        >>= fun cur_ctx ->
        let class_statement =
          Base.Option.value_exn (apply_parser Parser.Statement.statement class_string)
        in
        interpret_statement cur_ctx class_statement)
  ;;
end

let parse_and_run input =
  let open Statement in
  let open Interpret in
  let ctx_with_standard_classes =
    Base.Option.value_exn (Base.Result.ok (load_standard_classes empty_ctx))
  in
  match apply_parser initialize_block_statement input with
  | None -> Error EmptyProgram
  | Some statement ->
    (match interpret_statement ctx_with_standard_classes statement with
    | Error err -> Error err
    | Ok init_ctx ->
      let main_call =
        Base.Option.value_exn (apply_parser Parser.Expression.expression "main()")
      in
      interpret_expression init_ctx main_call)
;;
