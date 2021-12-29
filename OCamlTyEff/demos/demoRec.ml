open Ocaml_ty_eff_lib.Interpret
open Format

let () =
  printf
    "%a\n"
    pp_parse_and_run
    {|
let rec fix: (('a -['e]-> 'b) --> 'a -['e]-> 'b) -[Asgmt]-> 'a -['e]-> 'b = 
  fun (f : ('a -['e]-> 'b) --> 'a -['e]-> 'b) ->
    let r : ('a -['e]-> 'b) ref = ref (fun o : 'a -> (sneaky_eff raise1) ()) in
    let fixf : 'a -['e]-> 'b = fun x : 'a -> f !r x in
    let o: () = r := fixf in
    !r
;;
let fac: (int --> int) --> int --> int = fun self: (int --> int) -> fun n: int -> 
  match n <= 1 with
  | true -> 1
  | false -> n * (self (n - 1))
;;
let n : int = fix fac 6
;;
|}
;;
