open Ocaml_ty_eff_lib.Interpret
open Format

let () = printf "Raise1:\n%a\n\n" pp_parse_and_run {|
let exc: int = raise1 ()
|}

let () =
  printf
    "Raise1, catch1:\n%a\n\n"
    pp_parse_and_run
    {|
let exc: int = try raise1 () with
  | Exc1 -> 1
|}
;;

let () =
  printf
    "Raise1, catch2:\n%a\n\n"
    pp_parse_and_run
    {|
let exc: int = try raise1 () with
  | Exc2 -> 2
|}
;;

let () =
  printf
    "Raise1, catch both:\n%a\n"
    pp_parse_and_run
    {|
let exc: int = try raise1 () with
  | Exc1 -> 1
  | Exc2 -> 2
|}
;;
