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

(** Word parser *)
let word_p = take_while1 (fun c -> not (is_metachar c)) >>| fun s -> Word (s, [])

(** Assignment parser *)
let assignt_p =
  name_p
  >>= fun n ->
  char '=' *> option None (word_p >>| fun w -> Some w)
  >>| function
  | Some (Word (s, _)) ->
    AssigntStmt (n, Some (Word (s, [ ParameterExp; CommandSubst; ArithmExp; QuoteRem ])))
  | None -> AssigntStmt (n, None)
;;

(** Simple command parser *)
let cmd_p =
  blank *> many (assignt_p <* blank)
  >>= fun assignts ->
  many (word_p <* blank)
  <* blank
  >>= fun words ->
  let e_words =
    List.map
      (fun (Word (s, _)) ->
        Word
          ( s
          , [ BraceExp
            ; ParameterExp
            ; CommandSubst
            ; ArithmExp
            ; WordSpl
            ; FilenameExp
            ; QuoteRem
            ] ))
      words
  in
  match assignts, e_words with
  | _, hd :: tl -> return (Command (assignts, hd, tl))
  | hd :: tl, [] -> return (Assignt (hd, tl))
  | [], [] -> fail "Empty simple command"
;;

(* Tests *)

let test_cmd_p = test_p cmd_p
let fail_cmd_p = fail_p cmd_p

let%test _ =
  test_cmd_p
    "A=123"
    (Assignt
       ( AssigntStmt
           ( Name "A"
           , Some (Word ("123", [ ParameterExp; CommandSubst; ArithmExp; QuoteRem ])) )
       , [] ))
;;

let%test _ = test_cmd_p "A=" (Assignt (AssigntStmt (Name "A", None), []))

let%test _ =
  test_cmd_p
    "    A=123      B=567      _ckd24=df!5[]%$~7        "
    (Assignt
       ( AssigntStmt
           ( Name "A"
           , Some (Word ("123", [ ParameterExp; CommandSubst; ArithmExp; QuoteRem ])) )
       , [ AssigntStmt
             ( Name "B"
             , Some (Word ("567", [ ParameterExp; CommandSubst; ArithmExp; QuoteRem ])) )
         ; AssigntStmt
             ( Name "_ckd24"
             , Some
                 (Word ("df!5[]%$~7", [ ParameterExp; CommandSubst; ArithmExp; QuoteRem ]))
             )
         ] ))
;;

let%test _ =
  test_cmd_p
    "1A=123"
    (Command
       ( []
       , Word
           ( "1A=123"
           , [ BraceExp
             ; ParameterExp
             ; CommandSubst
             ; ArithmExp
             ; WordSpl
             ; FilenameExp
             ; QuoteRem
             ] )
       , [] ))
;;

let%test _ =
  test_cmd_p
    "cmd arg1 arg2"
    (Command
       ( []
       , Word
           ( "cmd"
           , [ BraceExp
             ; ParameterExp
             ; CommandSubst
             ; ArithmExp
             ; WordSpl
             ; FilenameExp
             ; QuoteRem
             ] )
       , [ Word
             ( "arg1"
             , [ BraceExp
               ; ParameterExp
               ; CommandSubst
               ; ArithmExp
               ; WordSpl
               ; FilenameExp
               ; QuoteRem
               ] )
         ; Word
             ( "arg2"
             , [ BraceExp
               ; ParameterExp
               ; CommandSubst
               ; ArithmExp
               ; WordSpl
               ; FilenameExp
               ; QuoteRem
               ] )
         ] ))
;;

let%test _ =
  test_cmd_p
    "    VAR1=123    VAR2=    cmd     arg1     arg2    "
    (Command
       ( [ AssigntStmt
             ( Name "VAR1"
             , Some (Word ("123", [ ParameterExp; CommandSubst; ArithmExp; QuoteRem ])) )
         ; AssigntStmt (Name "VAR2", None)
         ]
       , Word
           ( "cmd"
           , [ BraceExp
             ; ParameterExp
             ; CommandSubst
             ; ArithmExp
             ; WordSpl
             ; FilenameExp
             ; QuoteRem
             ] )
       , [ Word
             ( "arg1"
             , [ BraceExp
               ; ParameterExp
               ; CommandSubst
               ; ArithmExp
               ; WordSpl
               ; FilenameExp
               ; QuoteRem
               ] )
         ; Word
             ( "arg2"
             , [ BraceExp
               ; ParameterExp
               ; CommandSubst
               ; ArithmExp
               ; WordSpl
               ; FilenameExp
               ; QuoteRem
               ] )
         ] ))
;;
