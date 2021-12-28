open Core
open Kotlin_lib.Ast
open Kotlin_lib.Utils
open Kotlin_lib.Interpreter.Interpret (Result)
open Kotlin_lib.Parser
open Kotlin_lib.Parser.Statement

exception End_of_input

let help =
  {|
  Commands:
  # @help - get list of commands
  # @print_environment - print all records in environment
  # @last_eval_expression - print value of last evaluated expression
|}
;;

let interpert_repl_command ctx = function
  | "@help" -> print_endline help
  | "@print_environment" ->
    List.iter ctx.environment ~f:(fun rc -> print_endline (show_record_t rc))
  | "@last_eval_expression" -> print_endline (show_value ctx.last_eval_expression)
  | str -> printf "No command found [%s]\n" str
;;

let buffered_lines : string list ref = ref []

let rec repl ctx =
  try
    while true do
      printf ">> ";
      flush stdout;
      let line = input_line stdin in
      if String.is_prefix line ~prefix:"@"
      then interpert_repl_command ctx line
      else if String.is_suffix line ~suffix:";;"
      then (
        let filtered_line = String.slice line 0 (String.length line - 2) in
        buffered_lines := filtered_line :: !buffered_lines;
        raise End_of_input)
      else buffered_lines := line :: !buffered_lines
    done
  with
  | End_of_file ->
    print_endline "";
    repl ctx
  | End_of_input ->
    let acc_input =
      List.fold !buffered_lines ~init:"" ~f:(fun acc line -> line ^ "\n" ^ acc)
    in
    buffered_lines := [];
    let print_as_repl_answer str = printf "Kotlin REPL # %s\n" str in
    let parsed_input = apply_parser statement acc_input in
    let new_ctx =
      match parsed_input with
      | None ->
        print_as_repl_answer "Invalid input";
        ctx
      | Some stat ->
        (match interpret_statement ctx stat with
        | Error err ->
          print_as_repl_answer (show_error err);
          ctx
        | Ok eval_ctx ->
          (match stat with
          | Block _ | InitializeBlock _ ->
            print_as_repl_answer "Block expressions are not supported in REPL";
            ctx
          | Return _ ->
            print_as_repl_answer "Return expressions are not supported in REPL";
            ctx
          | Expression _ | Assign _ | AnonymousFunctionDeclarationStatement _ ->
            print_as_repl_answer (show_value eval_ctx.last_eval_expression);
            eval_ctx
          | If _ | While _ ->
            print_as_repl_answer "<REPL empty answer>";
            eval_ctx
          | VarDeclaration (_, _, name, _, _) ->
            let var = Option.value_exn (get_var_from_env eval_ctx.environment name) in
            print_as_repl_answer (show_record_t var);
            eval_ctx
          | FunDeclaration (_, name, _, _, _) | ClassDeclaration (_, name, _, _, _) ->
            let func =
              Option.value_exn (get_function_or_class_from_env eval_ctx.environment name)
            in
            print_as_repl_answer (show_record_t func);
            eval_ctx))
    in
    repl new_ctx
;;

let () = print_endline "Kotlin REPL"
let () = print_endline "Type @help for getting further information"

let start_repl =
  let ctx =
    { environment = []
    ; checked_not_null_values = []
    ; last_eval_expression = Unitialized
    ; last_return_value = Unitialized
    ; last_derefered_variable = None
    ; scope = Initialize
    }
  in
  repl (Option.value_exn (Base.Result.ok (load_standard_classes ctx)))
;;
