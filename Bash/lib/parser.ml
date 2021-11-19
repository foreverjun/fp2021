open Angstrom
open Ast

(* ---------- Interface ---------- *)

let parse = parse_string
(* TODO: create main parser function here *)

(* ---------- Common parsers ---------- *)

(** [chainl1 e op] parses one or more occurrences of [e], separated by [op].
Returns a value obtained by a left associative application of [op] to the values
returned by [e]. *)
let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

(* ---------- Basic Bash syntax --------- *)

let reserved =
  [ "if"
  ; "then"
  ; "elif"
  ; "else"
  ; "fi"
  ; "for"
  ; "in"
  ; "while"
  ; "do"
  ; "done"
  ; "case"
  ; "esac"
  ; "function"
  ; "!"
  ]
;;

let is_blank = function
  | ' ' | '\t' -> true
  | _ -> false
;;

let is_cmd_delim = function
  | '\n' | '\r' | ';' -> true
  | _ -> false
;;

let is_metachar = function
  | '|' | '&' | '(' | ')' | '<' | '>' -> true
  | c when is_blank c || is_cmd_delim c -> true
  | _ -> false
;;

let blank = take_while is_blank
let delim = take_while is_cmd_delim
let metachar = take_while is_metachar

(* ---------- Arithmetic ---------- *)

(* Operators *)
let plus = char '+' *> return (fun x y -> Plus (x, y))
let minus = char '-' *> return (fun x y -> Minus (x, y))
let mul = char '*' *> return (fun x y -> Mul (x, y))
let div = char '/' *> return (fun x y -> Div (x, y))
let less = char '<' *> return (fun x y -> Less (x, y))
let greater = char '>' *> return (fun x y -> Greater (x, y))
let lesseq = string "<=" *> return (fun x y -> LessEq (x, y))
let greatereq = string ">=" *> return (fun x y -> GreaterEq (x, y))
let equal = string "==" *> return (fun x y -> Equal (x, y))
let nequal = string "!=" *> return (fun x y -> NEqual (x, y))

(* Operands *)
let parens p = char '(' *> blank *> p <* blank <* char ')'

let num =
  option "" (string "+" <|> string "-")
  >>= fun sign ->
  take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
  >>| fun s -> Num (int_of_string (sign ^ s))
;;

(** Arithmetic parser *)
let arithm_p =
  fix (fun arithm_p ->
      let factor = blank *> (parens arithm_p <|> num) <* blank in
      let term = chainl1 factor (blank *> (mul <|> div) <* blank) in
      let expr = chainl1 term (blank *> (plus <|> minus) <* blank) in
      let comp =
        chainl1 expr (blank *> (lesseq <|> greatereq <|> less <|> greater) <* blank)
      in
      chainl1 comp (blank *> (equal <|> nequal) <* blank))
;;

(* Tests *)

let test_arithm_p s res =
  Result.get_ok (parse_string ~consume:Consume.All arithm_p s) = res
;;

let fail_arithm_p s =
  let _ = Result.get_error (parse_string ~consume:Consume.All arithm_p s) in
  true
;;

let%test _ = test_arithm_p "100" (Num 100)
let%test _ = test_arithm_p "   1 +     2" (Plus (Num 1, Num 2))
let%test _ = test_arithm_p "2 * 3 + 4" (Plus (Mul (Num 2, Num 3), Num 4))
let%test _ = test_arithm_p "(( (5)) )" (Num 5)

let%test _ =
  test_arithm_p
    "(1 < 2) + (3 >= 4) / 10"
    (Plus (Less (Num 1, Num 2), Div (GreaterEq (Num 3, Num 4), Num 10)))
;;

let%test _ =
  test_arithm_p
    "11 + 3 * 4 - 1 <= 17 / 9 != 5"
    (NEqual
       ( LessEq (Minus (Plus (Num 11, Mul (Num 3, Num 4)), Num 1), Div (Num 17, Num 9))
       , Num 5 ))
;;

let%test _ = fail_arithm_p "5 5"
let%test _ = fail_arithm_p "(()"
let%test _ = fail_arithm_p "+ -"
