open Bash_lib

let () =
  let s = Stdio.In_channel.input_all stdin in
  match Parser.parse_result s with
  | Ok script -> Ast.pp_script Format.std_formatter script
  | Error e -> Format.printf "Parsing error: %s" e
;;
