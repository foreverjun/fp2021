open Ocaml_ty_eff_lib.Interpret
open Format

let () =
  printf
    "Non exhaustive:\n%a\n\n"
    pp_parse_and_run
    {|
let n : int = match 1 :: [] with
| [] -> 0
| a :: b :: [] -> 1
|}
;;

let () = printf "Division by zero:\n%a\n" pp_parse_and_run {|
let n : int = 1 / 0
|}
