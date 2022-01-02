open Asm_lib

let () =
  let open Interpret.Eval (Interpret.Result) in
  let s = Stdio.In_channel.input_all stdin in
  match Parser.parse s with
  | Result.Ok ast -> (
    match asm ast with
    | Result.Ok env -> pp_env Format.std_formatter env
    | Error e -> print_string ("\nInterpret error " ^ e) )
  | Error e -> print_string ("\nParser error " ^ e)
