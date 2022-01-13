open Bash_lib

let interpret script =
  let open Interpreter.Eval (Interpreter.Result) in
  match ev_script empty_env script with
  | Ok env -> Printf.printf "Interpretation finished with return code: %i" env.retcode
  | Error e -> Printf.printf "Interpretation error: %s" e
;;

let () =
  let s = Stdio.In_channel.input_all stdin in
  match Parser.parse_result s with
  | Ok script ->
    let open Unix in
    let rd, wr = pipe () in
    let pid = fork () in
    if pid = 0
    then (
      (* Redirect stderr to the parent process *)
      dup2 wr stderr;
      interpret script;
      close wr)
    else (
      (* Wait for the child process to print anything to stderr *)
      let (_ : file_descr list * file_descr list * file_descr list) =
        select [ rd ] [] [] (-1.0)
      in
      kill pid Sys.sigint;
      let (_ : int * process_status) = waitpid [] pid in
      close rd)
  | Error e -> Printf.printf "Parsing error: %s" e
;;
