open Bash_lib

let () =
  let open Format in
  let s = Stdio.In_channel.input_all stdin in
  let run script =
    match Interpreter.interpret script with
    | Ok n -> printf "Interpretation finished with return code: %i" n
    | Error e -> printf "Interpretation error: %s" e
  in
  match Parser.parse s with
  | Ok script ->
    let open Unix in
    let rd, wr = pipe () in
    let pid = fork () in
    if pid = 0
    then (
      (* Redirect stderr to the parent process *)
      dup2 wr stderr;
      run script;
      close wr)
    else (
      (* Wait for the child process to print anything to stderr *)
      let (_ : file_descr list * file_descr list * file_descr list) =
        select [ rd ] [] [] (-1.0)
      in
      kill pid Sys.sigint;
      let (_ : int * process_status) = waitpid [] pid in
      close rd)
  | Error e -> printf "Parsing error: %s" e
;;
