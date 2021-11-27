open Angstrom
open Ast

(* -------------------- Interface -------------------- *)

let parse = parse_string
(* TODO: create main parser function here *)

(* -------------------- Common helper functions -------------------- *)

(** [chainl1 e op] parses one or more occurrences of [e], separated by [op].
Returns a value obtained by a left associative application of [op] to the values
returned by [e]. *)
let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let int_p =
  option "" (string "+" <|> string "-")
  >>= fun sign ->
  take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
  >>| fun s -> int_of_string (sign ^ s)
;;

(** Check if parser p returns result res on string s *)
let test_p p s res = Result.get_ok (parse_string ~consume:Consume.All p s) = res

(** Check if parser p fails on string s *)
let fail_p p s =
  try
    let _ = Result.get_error (parse_string ~consume:Consume.All p s) in
    true
  with
  | Invalid_argument _ -> false
;;

(* -------------------- Basic Bash syntax -------------------- *)

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

(* -------------------- Arithmetic -------------------- *)

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
let num = int_p >>| fun n -> Num n

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

let test_arithm_p = test_p arithm_p
let fail_arithm_p = fail_p arithm_p

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

(* -------------------- Word and expansions -------------------- *)

(** Word parser *)
let word_p = take_while1 (fun c -> not (is_metachar c)) >>| fun s -> Word s

(* Brace expansion *)

let brace_exp =
  let prefix =
    take_till (function
        | '{' -> true
        | c when is_metachar c -> true
        | _ -> false)
  in
  let seq =
    let elems_by p = p <* string ".." >>= fun s -> p >>| fun e -> s, e in
    let incr = option 1 (string ".." *> int_p >>| fun i -> max 1 (abs i)) in
    let range s e i =
      let rec up n acc = if n >= s then up (n - i) (n :: acc) else acc in
      let rec dn n acc = if n <= s then dn (n + i) (n :: acc) else acc in
      let act_e = s + ((e - s) / i * i) in
      if s <= e then up act_e [] else dn act_e []
    in
    let range_by f s e i = List.map f (range s e i) in
    elems_by (any_char >>| Char.code)
    >>= (function
          | s, e -> incr >>| range_by (fun c -> String.make 1 (Char.chr c)) s e)
    <|> (elems_by int_p
        >>= function
        | s, e -> incr >>| range_by (fun n -> string_of_int n) s e)
  in
  let strs =
    let str =
      take_while (function
          | ',' | '}' -> false
          | c when is_metachar c -> false
          | _ -> true)
    in
    sep_by1 (char ',') str
    >>= function
    | [ _ ] -> fail "Single string"
    | strs -> return strs
  in
  let postfix =
    take_till (function
        | c when is_metachar c -> true
        | _ -> false)
  in
  option "" prefix
  >>= fun pre ->
  char '{' *> (seq <|> strs)
  <* char '}'
  >>= fun body ->
  option "" postfix >>| fun post -> List.map (fun s -> pre ^ s ^ post) body
;;

(* Tests for brace expansion *)

let test_brace_exp = test_p brace_exp
let fail_brace_exp = fail_p brace_exp

let%test _ = test_brace_exp "ab{c,d,e}fd" [ "abcfd"; "abdfd"; "abefd" ]
let%test _ = test_brace_exp "ab{c,d,e}" [ "abc"; "abd"; "abe" ]
let%test _ = test_brace_exp "{c,d,e}fd" [ "cfd"; "dfd"; "efd" ]
let%test _ = test_brace_exp "ab{,,}fd" [ "abfd"; "abfd"; "abfd" ]
let%test _ = fail_brace_exp "ab{}fd"
let%test _ = fail_brace_exp "ab{c}fd"
let%test _ = test_brace_exp "1a{1..3}b5" [ "1a1b5"; "1a2b5"; "1a3b5" ]
let%test _ = test_brace_exp "1a{1..1}b5" [ "1a1b5" ]
let%test _ = test_brace_exp "1a{1..4..2}b5" [ "1a1b5"; "1a3b5" ]
let%test _ = test_brace_exp "1a{1..4..-2}b5" [ "1a1b5"; "1a3b5" ]
let%test _ = test_brace_exp "1a{1..4..0}b5" [ "1a1b5"; "1a2b5"; "1a3b5"; "1a4b5" ]
let%test _ = test_brace_exp "1a{3..1}b5" [ "1a3b5"; "1a2b5"; "1a1b5" ]
let%test _ = test_brace_exp "1a{-5..0..2}b5" [ "1a-5b5"; "1a-3b5"; "1a-1b5" ]
let%test _ = test_brace_exp "1a{d..a..2}b5" [ "1adb5"; "1abb5" ]
let%test _ = fail_brace_exp "1a{d..a..}b5"

(* -------------------- Simple command -------------------- *)

(** Name parser *)
let name_p =
  let is_name_beg = function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
    | _ -> false
  in
  let is_namechar = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false
  in
  peek_char
  >>= function
  | Some c when is_name_beg c -> take_while is_namechar >>| fun s -> Name s
  | _ -> fail "Name should begin with a letter or underscore"
;;

(** Assignment parser *)
let assignt_p =
  name_p
  >>= fun n ->
  char '='
  *> (char '(' *> blank *> sep_by blank word_p
     <* blank
     <* char ')'
     >>| (fun ws -> CompoundAssignt (n, ws))
     <|> (option None (word_p >>| fun w -> Some w) >>| fun w -> SimpleAssignt (n, w)))
;;

(** Simple command parser *)
let cmd_p =
  blank *> many (assignt_p <* blank)
  >>= fun assignts ->
  many (word_p <* blank)
  <* blank
  >>= fun words ->
  match assignts, words with
  | _, hd :: tl -> return (Command (assignts, hd, tl))
  | hd :: tl, [] -> return (Assignt (hd, tl))
  | [], [] -> fail "Empty simple command"
;;

(* Tests *)

let test_cmd_p = test_p cmd_p
let fail_cmd_p = fail_p cmd_p

let%test _ =
  test_cmd_p "A=123" (Assignt (SimpleAssignt (Name "A", Some (Word "123")), []))
;;

let%test _ = test_cmd_p "A=" (Assignt (SimpleAssignt (Name "A", None), []))

let%test _ =
  test_cmd_p
    "    A=123      B=567      _ckd24=df!5[]%$~7        "
    (Assignt
       ( SimpleAssignt (Name "A", Some (Word "123"))
       , [ SimpleAssignt (Name "B", Some (Word "567"))
         ; SimpleAssignt (Name "_ckd24", Some (Word "df!5[]%$~7"))
         ] ))
;;

let%test _ = test_cmd_p "1A=123" (Command ([], Word "1A=123", []))
let%test _ = test_cmd_p "ARR=()" (Assignt (CompoundAssignt (Name "ARR", []), []))

let%test _ =
  test_cmd_p
    "ARR=( 1   2  abc    )"
    (Assignt (CompoundAssignt (Name "ARR", [ Word "1"; Word "2"; Word "abc" ]), []))
;;

let%test _ =
  test_cmd_p
    "ARR1=( 1   2  abc    )        ARR2=(bcd)"
    (Assignt
       ( CompoundAssignt (Name "ARR1", [ Word "1"; Word "2"; Word "abc" ])
       , [ CompoundAssignt (Name "ARR2", [ Word "bcd" ]) ] ))
;;

let%test _ =
  test_cmd_p "cmd arg1 arg2" (Command ([], Word "cmd", [ Word "arg1"; Word "arg2" ]))
;;

let%test _ =
  test_cmd_p
    "    VAR1=123    VAR2=    cmd     arg1     arg2    "
    (Command
       ( [ SimpleAssignt (Name "VAR1", Some (Word "123"))
         ; SimpleAssignt (Name "VAR2", None)
         ]
       , Word "cmd"
       , [ Word "arg1"; Word "arg2" ] ))
;;
