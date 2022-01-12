open Kotlin_lib.Interpreter
open Stdio
open Kotlin_lib.Utils

let print_test_name name = printf "|-> test %s\n" name
let print_test_failed err = printf "Test failed: %s\n" (show_error err)

(**
  Пример программы, демонстрирующей, что в типы, не подразумевающие хранение null (например, Int), нельзя записывать null, а в типы, подразумевающие хранение null (например, Int?) можно
*)
let flow_sensitive_typing1 =
  {|

  fun main() {
    val nullVariable: Int? = null
    val x: Int = nullVariable
  }

  |}
;;

let () =
  print_test_name "flow_sensitive_typing1";
  match parse_and_run flow_sensitive_typing1 with
  | Ok _ -> ()
  | Error err -> print_test_failed err
;;

(**
  Пример программы, демонстрирующей, что неизменяемые переменные nullable типа можно проверить на null и, если его там нет, использовать их как переменные, которые не подразумевают хранение null
*)
let flow_sensitive_typing2 =
  {|

  fun main() {
    val notNullVariable: Int? = 1
    if(notNullVariable != null) {
      val x: Int = notNullVariable
      println("value of notNullVariable")
      println(x)
    }
  }

  |}
;;

let () =
  print_test_name "flow_sensitive_typing2";
  match parse_and_run flow_sensitive_typing2 with
  | Ok _ -> ()
  | Error err -> print_test_failed err
;;

(**
  Пример программы, демонстрирующей, что в функцию, которая принимает типы, не подразумевающие null, нельзя передать null
*)
let flow_sensitive_typing3 =
  {|

  fun fact(n: Int): Int {
    if(n > 1) return n * fact(n - 1)
    else return 1
  }

  fun main() {
    val nullVariable: Int? = null
    println(fact(nullVariable))
  }

  |}
;;

let () =
  print_test_name "flow_sensitive_typing3";
  match parse_and_run flow_sensitive_typing3 with
  | Ok _ -> ()
  | Error err -> print_test_failed err
;;

(**

  Пример программы, демонстрирующей, что неизменяемые переменные nullable типа можно проверить на null и, если его там нет, использовать их как переменные, которые не подразумевают хранение null (в параметрах функции)

*)
let flow_sensitive_typing4 =
  {|

  fun fact(n: Int): Int {
    if(n > 1) return n * fact(n - 1)
    else return 1
  }

  fun main() {
    val notNullVariable: Int? = 5
    if(notNullVariable != null) println(fact(notNullVariable))
  }

  |}
;;

let () =
  print_test_name "flow_sensitive_typing4";
  match parse_and_run flow_sensitive_typing4 with
  | Ok _ -> ()
  | Error err -> print_test_failed err
;;