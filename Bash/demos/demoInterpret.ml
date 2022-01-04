open Bash_lib

let () =
  let open Format in
  let s = Stdio.In_channel.input_all stdin in
  match Parser.parse s with
  | Ok script ->
    (match Interpreter.interpret script with
    | Ok n -> printf "\nInterpretation finished with return code: %i" n
    | Error e -> printf "Interpretation error: %s" e)
  | Error e -> printf "Parsing error: %s" e
;;
