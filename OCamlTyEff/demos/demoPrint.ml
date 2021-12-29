open Ocaml_ty_eff_lib.Interpret
open Format

let () =
  printf
    "%a\n"
    pp_parse_and_run
    {|
let rec iter : ('a -['e]-> unit) --> 'a list -['e]-> unit = fun (f: 'a -['e]-> unit) -> fun xs : 'a list ->
  match xs with
  | [] -> ()
  | x::xs -> let o: () = f x in iter f xs

let o: () = iter println ("1" :: "2" :: "3" :: [])
;;
|}
;;
