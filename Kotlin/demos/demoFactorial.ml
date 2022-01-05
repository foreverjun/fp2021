open Kotlin_lib.Interpreter
open Stdio
open Kotlin_lib.Utils

(**
  Пример программы с рекурсивным факториалом
*)
let factorial_algorithm =
  {|

  fun fact(n: Int): Int {
    if(n > 1) return n * fact(n - 1)
    else return 1
  }

  fun main() {
    println("Factorial of number 5:")
    println(fact(5))
  }

|}
;;

let () =
  match parse_and_run factorial_algorithm with
  | Ok _ -> ()
  | Error err -> printf "Test failed: %s" (show_error err)
;;