open Base
open Stdio
open Kotlin_lib.Ast
open Kotlin_lib.Utils
open Kotlin_lib.Interpreter.Interpret
open Kotlin_lib.Parser
open Kotlin_lib.Parser.Statement
open Kotlin_lib.Value_types

exception End_of_input of string

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
    List.iter (get_all_environment ctx) ~f:(fun rc -> print_endline (show_record_t rc))
  | "@last_eval_expression" ->
    print_endline (show_evaluated_expression ctx.last_eval_expression)
  | str -> printf "No command found [%s]\n" str
;;

let rec repl buffered_lines ctx =
  try
    printf ">> ";
    Out_channel.flush stdout;
    (* В случае, если внутри In_channel.stdin напечатать только EOF (ctrl + d), вылетит исключение и обработается в with блоке *)
    let line = Option.value_exn (In_channel.input_line In_channel.stdin) in
    if String.is_prefix line ~prefix:"@"
    then (
      interpert_repl_command ctx line;
      repl [] ctx)
    else if String.is_suffix line ~suffix:";;"
    then (
      let filtered_line = String.rstrip ?drop:(Some (fun c -> Char.equal c ';')) line in
      raise (End_of_input filtered_line))
    else repl (line :: buffered_lines) ctx
  with
  | End_of_input last_line ->
    let program = last_line :: buffered_lines in
    let acc_input = List.fold program ~init:"" ~f:(fun acc line -> line ^ "\n" ^ acc) in
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
          | Block _ ->
            print_as_repl_answer "Block expressions are not supported in REPL";
            ctx
          | Return _ ->
            print_as_repl_answer "Return expressions are not supported in REPL";
            ctx
          | InitInClass _ ->
            print_as_repl_answer "Init expressions are not supported in REPL";
            ctx
          | Expression _ | Assign _ ->
            print_as_repl_answer (show_evaluated_expression eval_ctx.last_eval_expression);
            eval_ctx
          | If _ | While _ | FunDeclaration _
          | ClassDeclaration (_, _, _, _, _)
          | VarDeclaration _ ->
            print_as_repl_answer "<REPL empty answer>";
            eval_ctx))
    in
    repl [] new_ctx
  | _ ->
    print_endline "";
    repl buffered_lines ctx
;;

let () = print_endline "Kotlin REPL"
let () = print_endline "Type @help for getting further information"

let start_repl =
  repl [] (Option.value_exn (Base.Result.ok (load_standard_classes empty_ctx)))
;;
