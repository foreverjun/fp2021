open Base
open Utils
open Parser
open Ast

module Interpret (M : MONAD_FAIL) = struct
  open M

  type record =
    | Variable of variable_t
    | Function of function_t
  [@@deriving show]

  type environment_record =
    { name : string
    ; mutable_status : bool
    ; content : record ref
    }
  [@@deriving show]

  type scope_type =
    | Initialize
    | Function

  (* стоит добавить вложенность environment-ов на локальный и глоабльный, чтобы ввести понятие глобальных переменных *)
  type context =
    { environment : (string, environment_record, String.comparator_witness) Map.t list
    ; last_eval_expr : value
    ; last_return_value : value
    ; scope : scope_type
    }

  let check_typename_value_correspondance = function
    | (Int | Nullable Int), IntValue _ -> true
    | (String | Nullable String), StringValue _ -> true
    | (Boolean | Nullable Boolean), BooleanValue _ -> true
    | Nullable _, NullValue -> true
    | _, _ -> false
  ;;

  let get_var_from_ctx ctx name =
    let rec get_var_from_env = function
      | [] -> None
      | hd :: tl -> if Map.mem hd name then Map.find hd name else get_var_from_env tl
    in
    get_var_from_env ctx.environment
  ;;

  let define_var_in_ctx ctx name mutable_status contents =
    match ctx.environment with
    | [] -> failwith "Should not reach here"
    | local_env :: other ->
      if not (Map.mem local_env name)
      then
        Some
          { ctx with
            environment =
              Map.set local_env name { name; mutable_status; content = { contents } }
              :: other
          }
      else None
  ;;

  let set_var_in_ctx ctx name mutable_status contents =
    match get_var_from_ctx ctx name with
    | None -> None
    | Some env_rec ->
      env_rec.content := contents;
      Some ctx
  ;;

  let rec run (ctx : context) statement = interpret_statement ctx statement

  and interpret_statement ctx = function
    | Block statements ->
      List.fold statements ~init:(M.return ctx) ~f:(fun monadic_ctx stat ->
          monadic_ctx
          >>= fun ctx ->
          if ctx.last_return_value == Unitialized
          then (
            match stat with
            | Block _ -> interpret_statement ctx stat >>= fun _ -> M.return ctx
            | _ -> interpret_statement ctx stat)
          else (
            match ctx.scope with
            | Function _ -> M.return ctx
            | _ -> M.fail ReturnNotInFunction))
    | Return expression ->
      interpret_expression ctx expression
      >>= fun ret_ctx -> M.return { ctx with last_return_value = ret_ctx.last_eval_expr }
    | Expression expression -> interpret_expression ctx expression
    (* TODO: разобраться с модификаторами у функций *)
    | FunDeclaration (mod_list, fun_name, arguments, fun_typename, fun_statement) ->
      (match fun_statement with
      | Block _ -> M.return fun_statement
      | _ -> M.fail FunctionBodyExpected)
      >>= fun statement ->
      (match
         define_var_in_ctx
           ctx
           fun_name
           false
           (Function { fun_typename; arguments; statement })
       with
      | None -> M.fail (Redeclaration fun_name)
      | Some new_ctx -> M.return new_ctx)
    (* TODO: разобраться с модификаторами у переменных *)
    | VarDeclaration (mod_list, var_modifier, var_name, var_typename, init_expression) ->
      (match init_expression with
      | None -> M.return Unitialized
      | Some expr ->
        interpret_expression ctx expr
        >>= fun interpreted_ctx -> M.return interpreted_ctx.last_eval_expr)
      >>= fun value ->
      (match
         define_var_in_ctx
           ctx
           var_name
           (var_modifier == Var)
           (Variable { var_typename; value })
       with
      | None -> M.fail (Redeclaration var_name)
      | Some new_ctx -> M.return new_ctx)
    (* исправить *)
    | Assign (identifier, assign_expression) ->
      interpret_expression ctx assign_expression
      >>= fun assign_value_ctx ->
      (match get_var_from_ctx ctx identifier with
      | None -> M.fail (UnknownVariable identifier)
      | Some rc ->
        (match !(rc.content) with
        | Function _ -> M.fail (VariableTypeMismatch identifier)
        | Variable var ->
          if rc.mutable_status
             && check_typename_value_correspondance
                  (var.var_typename, assign_value_ctx.last_eval_expr)
          then (
            match
              set_var_in_ctx
                assign_value_ctx
                identifier
                rc.mutable_status
                (Variable { var with value = assign_value_ctx.last_eval_expr })
            with
            | None -> M.fail (UnknownVariable identifier)
            | Some ctx -> M.return ctx)
          else
            M.fail
              (VariableValueTypeMismatch
                 (identifier, var.var_typename, assign_value_ctx.last_eval_expr))))
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
      (match get_var_from_ctx ctx identifier with
      | None -> M.fail (UnknownVariable identifier)
      | Some rc ->
        (match !(rc.content) with
        | Variable _ -> M.fail (ExpectedFunctionButFoundVariable identifier)
        | Function func ->
          let new_ctx =
            { ctx with
              environment = Map.empty (module String) :: ctx.environment
            ; scope = Function
            }
          in
          (match
             List.fold2
               func.arguments
               arg_expressions
               ~init:(M.return new_ctx)
               ~f:(fun monadic_ctx (arg_name, arg_typename) arg_expr ->
                 monadic_ctx
                 >>= fun monadic_ctx ->
                 interpret_expression ctx arg_expr
                 >>= fun eval_expr_ctx ->
                 if check_typename_value_correspondance
                      (arg_typename, eval_expr_ctx.last_eval_expr)
                 then (
                   match
                     define_var_in_ctx
                       monadic_ctx
                       arg_name
                       false
                       (Variable
                          { var_typename = arg_typename
                          ; value = eval_expr_ctx.last_eval_expr
                          })
                   with
                   | None -> M.fail (Redeclaration arg_name)
                   | Some ctx -> M.return ctx)
                 else M.fail (VariableTypeMismatch arg_name))
           with
          | Ok fun_ctx ->
            fun_ctx
            >>= fun fun_ctx ->
            interpret_statement fun_ctx func.statement
            >>= fun func_eval_ctx ->
            M.return { ctx with last_eval_expr = func_eval_ctx.last_return_value }
          | _ -> M.fail FunctionArgumentsCountMismatch)))
    | Const x -> M.return { ctx with last_eval_expr = x }
    | VarIdentifier x ->
      (match get_var_from_ctx ctx x with
      | None -> M.fail (UnknownVariable x)
      | Some rc ->
        (match !(rc.content) with
        | Function _ -> M.fail (VariableTypeMismatch x)
        | Variable var -> M.return { ctx with last_eval_expr = var.value }))
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
    { environment = [ Map.empty (module String) ]
    ; last_eval_expr = Unitialized
    ; last_return_value = Unitialized
    ; scope = Initialize
    }
  in
  match apply_parser statement input with
  | None -> Error EmptyProgram
  | Some statement -> run ctx statement
;;
