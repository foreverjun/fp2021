open Kotlin_lib.Interpreter
open Stdio
open Kotlin_lib.Utils

(**
  Пример программы с факториалом, которая не парсится из-за ";" в строке "var n: Int = m;"
*)
let factorial_algorithm =
  {|

  fun fact_seq(m: Int): Int {
    var acc: Int = 1
    var n: Int = m;
    while (n > 1) {
      acc = acc * n
      n = n - 1
    }
    return acc
  }

  fun main() {
    println("Factorial of number 5:")
    println(fact_seq(5))
  }

|}
;;

let () =
  match parse_and_run factorial_algorithm with
  | Ok _ -> ()
  | Error err -> printf "Test failed: %s" (show_error err)
;;
