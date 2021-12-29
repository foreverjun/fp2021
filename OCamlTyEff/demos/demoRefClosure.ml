open Ocaml_ty_eff_lib.Interpret
open Format

let () =
  printf
    "%a\n"
    pp_parse_and_run
    {|
let fresh_id : () -[Asgmt]-> int =
  let last_id : int ref = ref -1 in
  fun o : unit ->
    let o : unit = last_id := !last_id + 1 in
    !last_id
let a : int = fresh_id ()
let b : int = fresh_id ()
let c : int = fresh_id ()
;;
|}
;;
