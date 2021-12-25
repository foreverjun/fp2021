open Base
open Core
open Kotlin_lib.Ast
open Kotlin_lib.Utils
open Kotlin_lib.Interpreter.Interpret (Result)
open Kotlin_lib.Parser
open Kotlin_lib.Parser.Expression
open Kotlin_lib.Parser.Statement

let rec repl previous_input ctx =
  try
    printf ">> ";
    let input = read_line () in
    repl (previous_input ^ "\n" ^ input) ctx
  with
  | End_of_file ->
    let parsed_input = apply_parser statement previous_input in
    let new_ctx =
      match parsed_input with
      | None ->
        print_endline "Invalid input";
        ctx
      | Some stat ->
        (match interpret_statement ctx stat with
        | Error err ->
          print_endline (show_error err);
          ctx
        | Ok eval_ctx ->
          (match stat with
          | Block _ ->
            print_endline "Block expressions are not supported in REPL";
            ctx
          | Return _ ->
            print_endline "Return expressions are not supported in REPL";
            ctx
          | Expression _ | Assign _ | AnonymousFunctionDeclarationStatement _ ->
            print_endline (show_value eval_ctx.last_eval_expr);
            eval_ctx
          | If _ | While _ ->
            print_endline "Unit";
            eval_ctx
          | VarDeclaration (_, _, name, _, _) ->
            let var = Option.value_exn (get_var_from_env eval_ctx.environment name) in
            print_endline (show_record_t var);
            eval_ctx
          | FunDeclaration (_, name, _, _, _) | ClassDeclaration (_, name, _, _, _) ->
            let func =
              Option.value_exn (get_function_or_class_from_env eval_ctx.environment name)
            in
            print_endline (show_record_t func);
            eval_ctx
          | _ -> failwith ""))
    in
    repl "" new_ctx
;;

let () = print_endline "Kotlin REPL"

let start_repl =
  let ctx =
    { environment = []
    ; checked_not_null_values = []
    ; last_eval_expr = Unitialized
    ; last_return_value = Unitialized
    ; last_derefered_variable = None
    ; scope = Initialize
    }
  in
  repl "" ctx
;;
