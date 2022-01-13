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
  | Ok script -> interpret script
  | Error e -> Printf.printf "Parsing error: %s" e
;;
