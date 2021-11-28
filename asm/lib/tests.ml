open Parser
open Ast

let parse_test_suc pp p s exp =
  match parse p s with
  | Error _ -> false
  | Ok res when exp = res -> true
  | Ok res ->
    let open Format in
    print_string "\nActual is:\n";
    pp std_formatter res;
    false
;;

let parse_test_fail pp p s =
  match parse p s with
  | Error _ -> true
  | Result.Ok res ->
    let open Format in
    print_string "\nActual is:\n";
    pp std_formatter res;
    false
;;

let expr_test_suc = parse_test_suc pp_expr expr_p

let%test _ = expr_test_suc "10" (Const 10)
let%test _ = expr_test_suc "10 + 20" (Add (Const 10, Const 20))
let%test _ = expr_test_suc "10 + 10 * 19" (Add (Const 10, Mul (Const 10, Const 19)))
let%test _ = expr_test_suc "10 * 10 - 19" (Sub (Mul (Const 10, Const 10), Const 19))
let%test _ = expr_test_suc "1 * ( 0x10 << 19 )" (Mul (Const 1, Shl (Const 16, Const 19)))
let%test _ = expr_test_suc "(( 0x) ) " (Const 0)
let%test _ = expr_test_suc "(( RaX ) ) " (Reg "RAX")
let%test _ = expr_test_suc "(( RaX2 ) ) " (Label "RaX2")