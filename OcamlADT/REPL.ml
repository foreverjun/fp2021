open OcamlADT_lib.Ast
open Format
open OcamlADT_lib.Interpret
open OcamlADT_lib.Parser

let x = EVar "a"
let () = print_endline "REPL not implemented"

(* let () =
     match
       parse_with progr
         {|
           let x = function | Age age -> age | Name _ -> 0
       |}
     with
     | Ok ok -> printf "%a\n" pp_program ok
     | Error err -> printf "%s\n" err

   let () =
     match
       run_interpret
         {|
        type person = | Age of int | Name of string

        let misha = Age 19

        let mishaa = Name "Misha"

        let x = function
        | Age age -> age
        | Name _ -> 0

        let wqe = x misha
        let ewq = x mishaa
        let qqq = x (Age 2)
        let x y = match y with | Age age -> age | Name name -> name
        let wqe = x misha
        let ewq = x mishaa
        let qqq = x (Name "misha")
      |}
     with
     | Ok ok -> printf "%a\n" pp_interpret_ok ok
     | Error err -> printf "%a\n" pp_interpret_err err *)
