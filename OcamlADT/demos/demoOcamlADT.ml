open OcamlADT_lib.Interpret
open Format

let () =
  let str = Stdio.In_channel.input_all Caml.stdin in
  printf "%a$$$$\n" pp_run_interpret str
