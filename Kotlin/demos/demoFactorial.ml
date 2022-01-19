open Kotlin_lib.Interpreter
open Stdio
open Kotlin_lib.Utils

(**
  Пример программы с рекурсивным факториалом
*)
let factorial_algorithm =
  {|

  fun fixpoint(f: ((Int) -> Int, Int) -> Int): (Int) -> Int {
    return { eta: Int -> f(fixpoint(f), eta) }
  }

  fun fact_with_fixpoint(n: Int): Int {
    val fixpointed_fact: (Int) -> Int = fixpoint({self: (Int) -> Int, x: Int -> if(x <= 1) 1 else x * self(x - 1)})
    return fixpointed_fact(n)
  }

  fun fact(n: Int): Int {
    if(n > 1) return n * fact(n - 1)
    else return 1
  }

  fun fact_cps(n: Int, k: (Int) -> Int ): Int {
    if (n > 1)
      return fact_cps(n - 1, { l: Int -> k(l * n) } )
    else
      return k(1)
  }

  fun fact_seq(m: Int): Int {
    var acc: Int = 1
    var n: Int = m
    while (n > 1) {
      acc = acc * n
      n = n - 1
    }
    return acc
  }

  fun main() {
    println("Factorial of number 5:")
    println(fact(5))
    println(fact_cps(5, {n: Int -> n}))
    println(fact_seq(5))
    println(fact_with_fixpoint(5))
  }

|}
;;

let () =
  match parse_and_run factorial_algorithm with
  | Ok _ -> ()
  | Error err -> printf "Test failed: %s" (show_error err)
;;
