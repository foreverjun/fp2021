open Ocaml_ty_eff_lib.Interpret
open Format

let () =
  printf
    "%a\n"
    pp_parse_and_run
    {|
let rec map2 : ('a --> 'b -['e]-> 'c) --> 'a list --> 'b list -['e, exc Exc1]-> 'c list = 
  fun f: ('a --> 'b -['e]-> 'c) ->
    fun l1: 'a list -> fun l2: 'b list ->
  match (l1, l2) with
  | ([], []) -> []
  | (a1::l1, a2::l2) -> let r: 'c = f a1 a2 in r :: map2 f l1 l2
  | (o1, o2) -> raise1 ()

let combine: 'a list --> 'b list -[exc Exc1]-> ('a * 'b) list = 
  map2 (fun x : 'a -> fun y : 'b -> (x, y))

let l : (int * string) list = combine (1 :: 2 :: 3 :: []) ("1" :: "2" :: "3" :: [])
|}
;;
