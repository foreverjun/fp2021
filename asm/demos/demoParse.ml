open Asm_lib

let () =
  let s = Stdio.In_channel.input_all stdin in
  match Parser.parse s with
  | Result.Ok ast -> Ast.pp_directive Format.std_formatter ast
  | Error e -> print_string ("Error" ^ e)
