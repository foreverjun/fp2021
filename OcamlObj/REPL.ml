open Format
open OcamlObj_lib.Interpret




let () =
     match
       run_interpret
         {|
        let x = 1 :: 2 :: "p"
        |}
     with
     | Ok ok -> printf "%a\n" pp_interpret_ok ok
     | Error err -> printf "%a\n" pp_interpret_err err