open Ast
open Parser

(** Automatic tests *)

(* -------------------- Helper functions -------------------- *)

(** Check if parser p returns result res on string s *)
let succ_p pp p s exp =
  match parse p s with
  | Error e ->
    print_string ("Error: " ^ e ^ "\n");
    false
  | Ok res when exp = res -> true
  | Ok res ->
    let open Format in
    let fmt = std_formatter in
    pp_print_string fmt "\n-------------------- Input --------------------\n";
    pp_print_string fmt s;
    pp_print_string fmt "\n-------------------- Expected --------------------\n";
    pp fmt exp;
    pp_print_string fmt "\n-------------------- Actual --------------------\n";
    pp fmt res;
    pp_print_string fmt "\n-------------------- End --------------------\n";
    false
;;

(** Check if parser p fails on string s *)
let fail_p pp p s =
  match parse p s with
  | Error _ -> true
  | Ok res ->
    let open Format in
    let fmt = std_formatter in
    pp_print_string fmt "\n-------------------- Input --------------------\n";
    pp_print_string fmt (s ^ "\n");
    pp_print_string fmt "\n-------------------- Actual --------------------\n";
    pp fmt res;
    pp_print_string fmt "\n-------------------- End --------------------\n";
    false
;;

(* -------------------- Variable -------------------- *)

let succ_var_p = succ_p pp_var var_p
let fail_var_p = fail_p pp_var var_p

let%test _ = succ_var_p "VAR" (SimpleVar (Name "VAR"))
let%test _ = succ_var_p "_var" (SimpleVar (Name "_var"))
let%test _ = succ_var_p "ARR[hi there]" (Subscript (Name "ARR", "hi there"))
let%test _ = succ_var_p "ARR[ ]" (Subscript (Name "ARR", " "))
let%test _ = fail_var_p "321VAR"
let%test _ = fail_var_p "ARR[]"

(* -------------------- Arithmetic -------------------- *)

let succ_arithm_p = succ_p pp_arithm arithm_p
let fail_arithm_p = fail_p pp_arithm arithm_p

let%test _ = succ_arithm_p "100" (Num 100)
let%test _ = succ_arithm_p "   1 +     2" (Plus (Num 1, Num 2))
let%test _ = succ_arithm_p "2 * 3 + 4" (Plus (Mul (Num 2, Num 3), Num 4))
let%test _ = succ_arithm_p "(( (5)) )" (Num 5)

let%test _ =
  succ_arithm_p
    "(1 < 2) + (3 >= 4) / 10"
    (Plus (Less (Num 1, Num 2), Div (GreaterEq (Num 3, Num 4), Num 10)))
;;

let%test _ =
  succ_arithm_p
    "11 + 3 * 4 - 1 <= 17 / 9 != 5"
    (NEqual
       ( LessEq (Minus (Plus (Num 11, Mul (Num 3, Num 4)), Num 1), Div (Num 17, Num 9))
       , Num 5 ))
;;

let%test _ =
  succ_arithm_p
    "x + y + 1"
    (Plus (Plus (Var (SimpleVar (Name "x")), Var (SimpleVar (Name "y"))), Num 1))
;;

let%test _ = fail_arithm_p "5 5"
let%test _ = fail_arithm_p "(()"
let%test _ = fail_arithm_p "+ -"
let%test _ = fail_arithm_p "123ab"
let%test _ = fail_arithm_p "2 + 2 == 4))"

(* -------------------- Brace expansion -------------------- *)

let succ_brace_exp = succ_p pp_word brace_exp
let fail_brace_exp = fail_p pp_word brace_exp

let%test _ = succ_brace_exp "ab{c,d,e}fd" (BraceExp [ "abcfd"; "abdfd"; "abefd" ])
let%test _ = succ_brace_exp "ab{c,d,e}" (BraceExp [ "abc"; "abd"; "abe" ])
let%test _ = succ_brace_exp "{c,d,e}fd" (BraceExp [ "cfd"; "dfd"; "efd" ])
let%test _ = succ_brace_exp "ab{,,}fd" (BraceExp [ "abfd"; "abfd"; "abfd" ])
let%test _ = fail_brace_exp "ab{}fd"
let%test _ = fail_brace_exp "ab{c}fd"
let%test _ = succ_brace_exp "1a{1..3}b5" (BraceExp [ "1a1b5"; "1a2b5"; "1a3b5" ])
let%test _ = succ_brace_exp "1a{1..1}b5" (BraceExp [ "1a1b5" ])
let%test _ = succ_brace_exp "1a{1..4..2}b5" (BraceExp [ "1a1b5"; "1a3b5" ])
let%test _ = succ_brace_exp "1a{1..4..-2}b5" (BraceExp [ "1a1b5"; "1a3b5" ])

let%test _ =
  succ_brace_exp "1a{1..4..0}b5" (BraceExp [ "1a1b5"; "1a2b5"; "1a3b5"; "1a4b5" ])
;;

let%test _ = succ_brace_exp "1a{3..1}b5" (BraceExp [ "1a3b5"; "1a2b5"; "1a1b5" ])
let%test _ = succ_brace_exp "1a{-5..0..2}b5" (BraceExp [ "1a-5b5"; "1a-3b5"; "1a-1b5" ])
let%test _ = succ_brace_exp "1a{d..a..2}b5" (BraceExp [ "1adb5"; "1abb5" ])
let%test _ = fail_brace_exp "1a{d..a..}b5"

(* -------------------- Parameter expansion -------------------- *)

let succ_param_exp = succ_p pp_param_exp param_exp_p
let fail_param_exp = fail_p pp_param_exp param_exp_p

let%test _ = succ_param_exp "$ABC" (Param (SimpleVar (Name "ABC")))
let%test _ = succ_param_exp "${ABC}" (Param (SimpleVar (Name "ABC")))
let%test _ = succ_param_exp "${#ABC}" (Length (SimpleVar (Name "ABC")))
let%test _ = succ_param_exp "${ABC:-20}" (Substring (SimpleVar (Name "ABC"), -20, 0))
let%test _ = succ_param_exp "${ABC:5:5}" (Substring (SimpleVar (Name "ABC"), 5, 5))
let%test _ = succ_param_exp "${ABC#*.ml}" (CutMinBeg (SimpleVar (Name "ABC"), "*.ml"))
let%test _ = succ_param_exp "${ABC##*.ml}" (CutMaxBeg (SimpleVar (Name "ABC"), "*.ml"))
let%test _ = succ_param_exp "${ABC%*.ml}" (CutMinEnd (SimpleVar (Name "ABC"), "*.ml"))
let%test _ = succ_param_exp "${ABC%%*.ml}" (CutMaxEnd (SimpleVar (Name "ABC"), "*.ml"))
let%test _ = succ_param_exp "${ABC/a}" (SubstOne (SimpleVar (Name "ABC"), "a", ""))
let%test _ = succ_param_exp "${ABC/a/b}" (SubstOne (SimpleVar (Name "ABC"), "a", "b"))
let%test _ = succ_param_exp "${ABC//a}" (SubstAll (SimpleVar (Name "ABC"), "a", ""))
let%test _ = succ_param_exp "${ABC//a/b}" (SubstAll (SimpleVar (Name "ABC"), "a", "b"))
let%test _ = succ_param_exp "${ABC/#a}" (SubstBeg (SimpleVar (Name "ABC"), "a", ""))
let%test _ = succ_param_exp "${ABC/#a/b}" (SubstBeg (SimpleVar (Name "ABC"), "a", "b"))
let%test _ = succ_param_exp "${ABC/%a}" (SubstEnd (SimpleVar (Name "ABC"), "a", ""))
let%test _ = succ_param_exp "${ABC/%a/b}" (SubstEnd (SimpleVar (Name "ABC"), "a", "b"))

(* -------------------- Command substitution -------------------- *)

let succ_cmd_subst = succ_p pp_word (inn_cmd_subst ())
let fail_cmd_subst = fail_p pp_word (inn_cmd_subst ())

let%test _ =
  succ_cmd_subst
    "$(X=2)"
    (CmdSubst (Assignt (SimpleAssignt (SimpleVar (Name "X"), Some (Word "2")), [])))
;;

let%test _ =
  succ_cmd_subst "$(echo hey)" (CmdSubst (Command ([], Word "echo", [ Word "hey" ])))
;;

let%test _ = fail_cmd_subst "$(echo hey"
let%test _ = fail_cmd_subst "$X=2)"

(* -------------------- Arithmetic expansion -------------------- *)

let succ_arithm_exp = succ_p pp_word arithm_exp
let fail_arithm_exp = fail_p pp_word arithm_exp

let%test _ =
  succ_arithm_exp "$((2 + 2 == 4))" (ArithmExp (Equal (Plus (Num 2, Num 2), Num 4)))
;;

let%test _ = fail_arithm_exp "$((2 + 2 == 4)"
let%test _ = fail_arithm_exp "$((2 + 2 == 4"
let%test _ = fail_arithm_exp "$(2 + 2 == 4))"
let%test _ = fail_arithm_exp "$2 + 2 == 4))"
let%test _ = fail_arithm_exp "$(2 + 2 == 4)"

(* -------------------- Word with expansions -------------------- *)

let succ_word_p ?(b = true) ?(p = true) ?(c = true) ?(a = true) ?(f = true) =
  succ_p pp_word (word_p ~brc:b ~prm:p ~cmd:c ~ari:a ~fln:f ())
;;

let fail_word_p ?(b = true) ?(p = true) ?(c = true) ?(a = true) ?(f = true) =
  fail_p pp_word (word_p ~brc:b ~prm:p ~cmd:c ~ari:a ~fln:f ())
;;

let%test _ = succ_word_p "something" (Word "something")
let%test _ = succ_word_p "1{a,b}2" (BraceExp [ "1a2"; "1b2" ])
let%test _ = succ_word_p "$A" (ParamExp (Param (SimpleVar (Name "A"))))

let%test _ =
  succ_word_p "$(cmd arg)" (CmdSubst (Command ([], Word "cmd", [ Word "arg" ])))
;;

let%test _ = succ_word_p "$((3 / 1))" (ArithmExp (Div (Num 3, Num 1)))

(* -------------------- Simple command -------------------- *)

let succ_cmd_p = succ_p pp_cmd cmd_p
let fail_cmd_p = fail_p pp_cmd cmd_p

let%test _ =
  succ_cmd_p
    "A=123"
    (Assignt (SimpleAssignt (SimpleVar (Name "A"), Some (Word "123")), []))
;;

let%test _ = succ_cmd_p "A=" (Assignt (SimpleAssignt (SimpleVar (Name "A"), None), []))

let%test _ =
  succ_cmd_p
    "A=123      B=567      _ckd24=df!5[]%$~7"
    (Assignt
       ( SimpleAssignt (SimpleVar (Name "A"), Some (Word "123"))
       , [ SimpleAssignt (SimpleVar (Name "B"), Some (Word "567"))
         ; SimpleAssignt (SimpleVar (Name "_ckd24"), Some (Word "df!5[]%$~7"))
         ] ))
;;

let%test _ = succ_cmd_p "1A=123" (Command ([], Word "1A=123", []))

let%test _ =
  succ_cmd_p
    "ARR[3]=123"
    (Assignt (SimpleAssignt (Subscript (Name "ARR", "3"), Some (Word "123")), []))
;;

let%test _ =
  succ_cmd_p "ARR=()" (Assignt (CompoundAssignt (SimpleVar (Name "ARR"), []), []))
;;

let%test _ =
  succ_cmd_p
    "ARR=( 1   2  abc    )"
    (Assignt
       (CompoundAssignt (SimpleVar (Name "ARR"), [ Word "1"; Word "2"; Word "abc" ]), []))
;;

let%test _ =
  succ_cmd_p
    "ARR1=( 1   2  abc    )        ARR2=(bcd)"
    (Assignt
       ( CompoundAssignt (SimpleVar (Name "ARR1"), [ Word "1"; Word "2"; Word "abc" ])
       , [ CompoundAssignt (SimpleVar (Name "ARR2"), [ Word "bcd" ]) ] ))
;;

let%test _ =
  succ_cmd_p "cmd arg1 arg2" (Command ([], Word "cmd", [ Word "arg1"; Word "arg2" ]))
;;

let%test _ =
  succ_cmd_p
    "VAR1=123    VAR2=    cmd     arg1     arg2"
    (Command
       ( [ SimpleAssignt (SimpleVar (Name "VAR1"), Some (Word "123"))
         ; SimpleAssignt (SimpleVar (Name "VAR2"), None)
         ]
       , Word "cmd"
       , [ Word "arg1"; Word "arg2" ] ))
;;
