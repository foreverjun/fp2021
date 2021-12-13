open Ast
open Parser

(** Automatic tests *)

(* -------------------- Helper functions -------------------- *)

(** Parse string s with parser p *)
let test_parse p s = Angstrom.parse_string ~consume:All p s

(** Check if parser p returns result res on string s *)
let succ_p pp p s exp =
  match test_parse p s with
  | Error e ->
    print_string ("Error: " ^ e ^ "\n");
    false
  | Ok res when exp = res -> true
  | Ok res ->
    let open Format in
    let fmt = std_formatter in
    pp_print_string fmt "\n-------------------- Input --------------------\n";
    pp_print_string fmt s;
    pp_print_string fmt "\n------------------- Expected ------------------\n";
    pp fmt exp;
    pp_print_string fmt "\n-------------------- Actual -------------------\n";
    pp fmt res;
    pp_print_string fmt "\n-----------------------------------------------\n";
    false
;;

(** Check if parser p fails on string s *)
let fail_p pp p s =
  match test_parse p s with
  | Error _ -> true
  | Ok res ->
    let open Format in
    let fmt = std_formatter in
    pp_print_string fmt "\n-------------------- Input --------------------\n";
    pp_print_string fmt (s ^ "\n");
    pp_print_string fmt "\n-------------------- Actual -------------------\n";
    pp fmt res;
    pp_print_string fmt "\n-----------------------------------------------\n";
    false
;;

(* -------------------- Variable -------------------- *)

let succ_var_p = succ_p pp_var var_p
let fail_var_p = fail_p pp_var var_p

let%test _ = succ_var_p "VAR" (SimpleVar (Name "VAR"))
let%test _ = succ_var_p "_var" (SimpleVar (Name "_var"))
let%test _ = succ_var_p "ARR[hi there]" (Subscript (Name "ARR", "hi there"))
let%test _ = succ_var_p "ARR[ ]" (Subscript (Name "ARR", " "))
let%test _ = fail_var_p " VAR"
let%test _ = fail_var_p "VAR "
let%test _ = fail_var_p " VAR "
let%test _ = fail_var_p "321VAR"
let%test _ = fail_var_p "ARR[]"

(* -------------------- Arithmetic -------------------- *)

let succ_arithm_p = succ_p pp_arithm arithm_p
let fail_arithm_p = fail_p pp_arithm arithm_p

let%test _ = succ_arithm_p "100" (Num 100)
let%test _ = succ_arithm_p "1 +     2" (Plus (Num 1, Num 2))
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

let%test _ = fail_arithm_p " 100"
let%test _ = fail_arithm_p "100 "
let%test _ = fail_arithm_p " 100 "
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
let%test _ = fail_brace_exp " ab{c,d,e}fd"
let%test _ = fail_brace_exp "ab{c,d,e}fd "
let%test _ = fail_brace_exp " ab{c,d,e}fd "
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
let%test _ = fail_param_exp " $ABC"
let%test _ = fail_param_exp "$ABC "
let%test _ = fail_param_exp " $ABC "

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

let%test _ = fail_cmd_subst " $(X=2)"
let%test _ = fail_cmd_subst "$(X=2) "
let%test _ = fail_cmd_subst " $(X=2) "
let%test _ = fail_cmd_subst "$(echo hey"
let%test _ = fail_cmd_subst "$X=2)"

(* -------------------- Arithmetic expansion -------------------- *)

let succ_arithm_exp = succ_p pp_word arithm_exp
let fail_arithm_exp = fail_p pp_word arithm_exp

let%test _ =
  succ_arithm_exp "$((2 + 2 == 4))" (ArithmExp (Equal (Plus (Num 2, Num 2), Num 4)))
;;

let%test _ =
  succ_arithm_exp "$(( 2 + 2 == 4  ))" (ArithmExp (Equal (Plus (Num 2, Num 2), Num 4)))
;;

let%test _ = fail_arithm_exp " $((2 + 2 == 4))"
let%test _ = fail_arithm_exp "$((2 + 2 == 4)) "
let%test _ = fail_arithm_exp " $((2 + 2 == 4)) "
let%test _ = fail_arithm_exp "$((2 + 2 == 4)"
let%test _ = fail_arithm_exp "$((2 + 2 == 4"
let%test _ = fail_arithm_exp "$(2 + 2 == 4))"
let%test _ = fail_arithm_exp "$2 + 2 == 4))"
let%test _ = fail_arithm_exp "$(2 + 2 == 4)"

(* -------------------- Filename expansion -------------------- *)

let succ_filename_exp = succ_p pp_word filename_exp
let fail_filename_exp = fail_p pp_word filename_exp

let%test _ = succ_filename_exp "?.ml" (FilenameExp "?.ml")
let%test _ = succ_filename_exp "*.txt" (FilenameExp "*.txt")
let%test _ = succ_filename_exp "[" (FilenameExp "[")
let%test _ = succ_filename_exp "[?*" (FilenameExp "[?*")
let%test _ = fail_filename_exp " ?.ml"
let%test _ = fail_filename_exp "?.ml "
let%test _ = fail_filename_exp " ?.ml "
let%test _ = fail_filename_exp "]"
let%test _ = fail_filename_exp "abc.ml"

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
let%test _ = succ_word_p "?.a" (FilenameExp "?.a")
let%test _ = fail_word_p " something"
let%test _ = fail_word_p "something "
let%test _ = fail_word_p " something "
let%test _ = fail_word_p "if"
let%test _ = succ_word_p ~b:false "1{a,b}2" (Word "1{a,b}2")
let%test _ = succ_word_p ~f:false "?.a" (Word "?.a")

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

let%test _ = fail_cmd_p " A=123"
let%test _ = fail_cmd_p "A=123 "
let%test _ = fail_cmd_p " A=123 "
let%test _ = fail_cmd_p " echo 1"
let%test _ = fail_cmd_p "echo 1 "
let%test _ = fail_cmd_p " echo 1 "

(* -------------------- Redirection -------------------- *)

let succ_redir_p = succ_p pp_redir redir_p
let fail_redir_p = fail_p pp_redir redir_p

let%test _ = succ_redir_p "< abc" (RedirInp (0, Word "abc"))
let%test _ = succ_redir_p "> abc" (RedirOtp (1, Word "abc"))
let%test _ = succ_redir_p ">> abc" (AppendOtp (1, Word "abc"))
let%test _ = succ_redir_p "<& abc" (DuplInp (0, Word "abc"))
let%test _ = succ_redir_p ">& abc" (DuplOtp (1, Word "abc"))
let%test _ = succ_redir_p "12<abc" (RedirInp (12, Word "abc"))
let%test _ = succ_redir_p "12< abc" (RedirInp (12, Word "abc"))
let%test _ = fail_redir_p " < abc"
let%test _ = fail_redir_p "< abc "
let%test _ = fail_redir_p " < abc "
let%test _ = fail_redir_p "12 < abc"
let%test _ = succ_redir_p "< $a" (RedirInp (0, ParamExp (Param (SimpleVar (Name "a")))))

(* -------------------- Pipeline list -------------------- *)

let succ_pipeline_list_p = succ_p pp_pipeline_list pipeline_list_p
let fail_pipeline_list_p = fail_p pp_pipeline_list pipeline_list_p

let%test _ =
  succ_pipeline_list_p
    "echo 1"
    (SinglePipeline
       (Pipeline (false, SimpleCommand (Command ([], Word "echo", [ Word "1" ]), []), [])))
;;

let%test _ =
  succ_pipeline_list_p
    "echo 1 && echo 2"
    (PipelineAndList
       ( Pipeline (false, SimpleCommand (Command ([], Word "echo", [ Word "1" ]), []), [])
       , SinglePipeline
           (Pipeline
              (false, SimpleCommand (Command ([], Word "echo", [ Word "2" ]), []), [])) ))
;;

let%test _ =
  succ_pipeline_list_p
    "! echo 1 && ! echo 2"
    (PipelineAndList
       ( Pipeline (true, SimpleCommand (Command ([], Word "echo", [ Word "1" ]), []), [])
       , SinglePipeline
           (Pipeline
              (true, SimpleCommand (Command ([], Word "echo", [ Word "2" ]), []), [])) ))
;;

let%test _ =
  succ_pipeline_list_p
    "echo 1 || echo 2"
    (PipelineOrList
       ( Pipeline (false, SimpleCommand (Command ([], Word "echo", [ Word "1" ]), []), [])
       , SinglePipeline
           (Pipeline
              (false, SimpleCommand (Command ([], Word "echo", [ Word "2" ]), []), [])) ))
;;

let%test _ =
  succ_pipeline_list_p
    "echo 1 && echo 2 || echo 3"
    (PipelineAndList
       ( Pipeline (false, SimpleCommand (Command ([], Word "echo", [ Word "1" ]), []), [])
       , PipelineOrList
           ( Pipeline
               (false, SimpleCommand (Command ([], Word "echo", [ Word "2" ]), []), [])
           , SinglePipeline
               (Pipeline
                  (false, SimpleCommand (Command ([], Word "echo", [ Word "3" ]), []), []))
           ) ))
;;

let%test _ =
  succ_pipeline_list_p
    "echo 1 || echo 2 && echo 3"
    (PipelineOrList
       ( Pipeline (false, SimpleCommand (Command ([], Word "echo", [ Word "1" ]), []), [])
       , PipelineAndList
           ( Pipeline
               (false, SimpleCommand (Command ([], Word "echo", [ Word "2" ]), []), [])
           , SinglePipeline
               (Pipeline
                  (false, SimpleCommand (Command ([], Word "echo", [ Word "3" ]), []), []))
           ) ))
;;

let%test _ = fail_pipeline_list_p " echo 1"
let%test _ = fail_pipeline_list_p "echo 1 "
let%test _ = fail_pipeline_list_p " echo 1 "

(* -------------------- Pipeline -------------------- *)

let succ_pipeline_p = succ_p pp_pipeline pipeline_p
let fail_pipeline_p = fail_p pp_pipeline pipeline_p

let%test _ =
  succ_pipeline_p
    "echo 1"
    (Pipeline (false, SimpleCommand (Command ([], Word "echo", [ Word "1" ]), []), []))
;;

let%test _ =
  succ_pipeline_p
    "! echo 1"
    (Pipeline (true, SimpleCommand (Command ([], Word "echo", [ Word "1" ]), []), []))
;;

let%test _ =
  succ_pipeline_p
    "!echo 1"
    (Pipeline (false, SimpleCommand (Command ([], Word "!echo", [ Word "1" ]), []), []))
;;

let%test _ =
  succ_pipeline_p
    "echo 1 | grep 1"
    (Pipeline
       ( false
       , SimpleCommand (Command ([], Word "echo", [ Word "1" ]), [])
       , [ SimpleCommand (Command ([], Word "grep", [ Word "1" ]), []) ] ))
;;

let%test _ =
  succ_pipeline_p
    "while a; do meow; done 2>& 1 | grep 1 >> a.txt"
    (Pipeline
       ( false
       , While
           ( WhileLoop
               ( SinglePipeline
                   (Pipeline (false, SimpleCommand (Command ([], Word "a", []), []), []))
               , SinglePipeline
                   (Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), []))
               )
           , [ DuplOtp (2, Word "1") ] )
       , [ SimpleCommand
             (Command ([], Word "grep", [ Word "1" ]), [ AppendOtp (1, Word "a.txt") ])
         ] ))
;;

let%test _ = fail_pipeline_p " echo 1"
let%test _ = fail_pipeline_p "echo 1 "
let%test _ = fail_pipeline_p " echo 1 "

(* -------------------- Compound -------------------- *)

let succ_compound_p = succ_p pp_compound compound_p
let fail_compound_p = fail_p pp_compound compound_p

let%test _ =
  succ_compound_p
    "while a; do meow; done"
    (While
       ( WhileLoop
           ( SinglePipeline
               (Pipeline (false, SimpleCommand (Command ([], Word "a", []), []), []))
           , SinglePipeline
               (Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), []))
           )
       , [] ))
;;

let%test _ =
  succ_compound_p
    "while a; do meow; done >> a.txt"
    (While
       ( WhileLoop
           ( SinglePipeline
               (Pipeline (false, SimpleCommand (Command ([], Word "a", []), []), []))
           , SinglePipeline
               (Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), []))
           )
       , [ AppendOtp (1, Word "a.txt") ] ))
;;

let%test _ =
  succ_compound_p
    "while a; do meow; done>>a.txt"
    (While
       ( WhileLoop
           ( SinglePipeline
               (Pipeline (false, SimpleCommand (Command ([], Word "a", []), []), []))
           , SinglePipeline
               (Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), []))
           )
       , [ AppendOtp (1, Word "a.txt") ] ))
;;

let%test _ =
  succ_compound_p
    "while a; do meow; done >> a.txt 2>& 1"
    (While
       ( WhileLoop
           ( SinglePipeline
               (Pipeline (false, SimpleCommand (Command ([], Word "a", []), []), []))
           , SinglePipeline
               (Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), []))
           )
       , [ AppendOtp (1, Word "a.txt"); DuplOtp (2, Word "1") ] ))
;;

let%test _ =
  succ_compound_p
    "for i in 1 2 34; do meow; done >> a.txt"
    (For
       ( ListFor
           ( Name "i"
           , [ Word "1"; Word "2"; Word "34" ]
           , SinglePipeline
               (Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), []))
           )
       , [ AppendOtp (1, Word "a.txt") ] ))
;;

let%test _ =
  succ_compound_p
    "if ((1)); then meow; fi >> a.txt"
    (If
       ( IfStmt
           ( SinglePipeline (Pipeline (false, ArithmExpr (Num 1, []), []))
           , SinglePipeline
               (Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), []))
           , None )
       , [ AppendOtp (1, Word "a.txt") ] ))
;;

let%test _ =
  succ_compound_p
    "case abc in esac >> a.txt"
    (Case (CaseStmt (Word "abc", []), [ AppendOtp (1, Word "a.txt") ]))
;;

let%test _ = fail_compound_p " while a; do meow; done"
let%test _ = fail_compound_p "while a; do meow; done "
let%test _ = fail_compound_p " while a; do meow; done "
let%test _ = fail_compound_p " case abc in esac >> a.txt"
let%test _ = fail_compound_p "case abc in esac >> a.txt "
let%test _ = fail_compound_p " case abc in esac >> a.txt "

(* -------------------- While -------------------- *)

let succ_while_loop_p = succ_p pp_while_loop while_loop_p
let fail_while_loop_p = fail_p pp_while_loop while_loop_p

let%test _ =
  succ_while_loop_p
    "while a; do echo a; done"
    (WhileLoop
       ( SinglePipeline
           (Pipeline (false, SimpleCommand (Command ([], Word "a", []), []), []))
       , SinglePipeline
           (Pipeline
              (false, SimpleCommand (Command ([], Word "echo", [ Word "a" ]), []), [])) ))
;;

let%test _ =
  succ_while_loop_p
    "while a \n do echo a \n done"
    (WhileLoop
       ( SinglePipeline
           (Pipeline (false, SimpleCommand (Command ([], Word "a", []), []), []))
       , SinglePipeline
           (Pipeline
              (false, SimpleCommand (Command ([], Word "echo", [ Word "a" ]), []), [])) ))
;;

let%test _ =
  succ_while_loop_p
    "while a ;\n\n do echo a; done"
    (WhileLoop
       ( SinglePipeline
           (Pipeline (false, SimpleCommand (Command ([], Word "a", []), []), []))
       , SinglePipeline
           (Pipeline
              (false, SimpleCommand (Command ([], Word "echo", [ Word "a" ]), []), [])) ))
;;

let%test _ = fail_while_loop_p " while a; do echo a; done"
let%test _ = fail_while_loop_p "while a; do echo a; done "
let%test _ = fail_while_loop_p " while a; do echo a; done "
let%test _ = fail_while_loop_p "while a\n; do echo a; done"
let%test _ = fail_while_loop_p "while a; do echo a\n; done"
let%test _ = fail_while_loop_p "while a do echo a done"
let%test _ = fail_while_loop_p "while a; do echo a;"
let%test _ = fail_while_loop_p "while a; do echo a"
let%test _ = fail_while_loop_p "while a; do echo a done"
let%test _ = fail_while_loop_p "while a do echo a; done"
let%test _ = fail_while_loop_p "while a; echo a; done"

(* -------------------- For -------------------- *)

let succ_for_loop_p = succ_p pp_for_loop for_loop_p
let fail_for_loop_p = fail_p pp_for_loop for_loop_p

let%test _ =
  succ_for_loop_p
    "for i in 1 2 34; do meow; done"
    (ListFor
       ( Name "i"
       , [ Word "1"; Word "2"; Word "34" ]
       , SinglePipeline
           (Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), [])) ))
;;

let%test _ =
  succ_for_loop_p
    "for i in 1 2 34;\n\n do meow;\n done"
    (ListFor
       ( Name "i"
       , [ Word "1"; Word "2"; Word "34" ]
       , SinglePipeline
           (Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), [])) ))
;;

let%test _ =
  succ_for_loop_p
    "for i in; do meow; done"
    (ListFor
       ( Name "i"
       , []
       , SinglePipeline
           (Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), [])) ))
;;

let%test _ = fail_for_loop_p " for i in 1 2 34; do meow; done"
let%test _ = fail_for_loop_p "for i in 1 2 34; do meow; done "
let%test _ = fail_for_loop_p " for i in 1 2 34; do meow; done "
let%test _ = fail_for_loop_p "for  in 1 2 3; do meow; done"
let%test _ = fail_for_loop_p "for i in 1 2 3\n; do meow\n; done"
let%test _ = fail_for_loop_p "for i in 1 2 3; do meow done"
let%test _ = fail_for_loop_p "for i in 1 2 3; do meow;"
let%test _ = fail_for_loop_p "for i in 1 2 3 do meow; done"
let%test _ = fail_for_loop_p "for i in 1 2 3; meow; done"

let%test _ =
  succ_for_loop_p
    "for ((0; 0; 1 + 1)); do meow; done"
    (ExprFor
       ( Num 0
       , Num 0
       , Plus (Num 1, Num 1)
       , SinglePipeline
           (Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), [])) ))
;;

let%test _ =
  succ_for_loop_p
    "for (( 0; 0; 1 + 1 )) ; do meow; done"
    (ExprFor
       ( Num 0
       , Num 0
       , Plus (Num 1, Num 1)
       , SinglePipeline
           (Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), [])) ))
;;

let%test _ =
  succ_for_loop_p
    "for ((;;)) ; do meow; done"
    (ExprFor
       ( Num 1
       , Num 1
       , Num 1
       , SinglePipeline
           (Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), [])) ))
;;

let%test _ = fail_for_loop_p " for ((0; 0; 1 + 1)); do meow; done"
let%test _ = fail_for_loop_p "for ((0; 0; 1 + 1)); do meow; done "
let%test _ = fail_for_loop_p " for ((0; 0; 1 + 1)); do meow; done "
let%test _ = fail_for_loop_p "for (0; 0; 1 + 1)); do meow; done"
let%test _ = fail_for_loop_p "for ((0; 0; 1 + 1); do meow; done"
let%test _ = fail_for_loop_p "for ((0; 0; 1 + 1)) do meow; done"
let%test _ = fail_for_loop_p "for ((0; 1 + 1)); do meow; done"
let%test _ = fail_for_loop_p "for ((;;0; 1 + 1)); do meow; done"

(* -------------------- If -------------------- *)

let succ_if_stmt_p = succ_p pp_if_stmt if_stmt_p
let fail_if_stmt_p = fail_p pp_if_stmt if_stmt_p

let%test _ =
  succ_if_stmt_p
    "if ((1)); then echo 1; else echo 0; fi"
    (IfStmt
       ( SinglePipeline (Pipeline (false, ArithmExpr (Num 1, []), []))
       , SinglePipeline
           (Pipeline
              (false, SimpleCommand (Command ([], Word "echo", [ Word "1" ]), []), []))
       , Some
           (SinglePipeline
              (Pipeline
                 (false, SimpleCommand (Command ([], Word "echo", [ Word "0" ]), []), [])))
       ))
;;

let%test _ =
  succ_if_stmt_p
    "if ((1)); then echo 1; fi"
    (IfStmt
       ( SinglePipeline (Pipeline (false, ArithmExpr (Num 1, []), []))
       , SinglePipeline
           (Pipeline
              (false, SimpleCommand (Command ([], Word "echo", [ Word "1" ]), []), []))
       , None ))
;;

let%test _ =
  succ_if_stmt_p
    "if ((1)) ;\n\n then echo 1;\n fi"
    (IfStmt
       ( SinglePipeline (Pipeline (false, ArithmExpr (Num 1, []), []))
       , SinglePipeline
           (Pipeline
              (false, SimpleCommand (Command ([], Word "echo", [ Word "1" ]), []), []))
       , None ))
;;

let%test _ = fail_if_stmt_p " if ((1)); then echo 1; else echo 0; fi"
let%test _ = fail_if_stmt_p "if ((1)); then echo 1; else echo 0; fi "
let%test _ = fail_if_stmt_p " if ((1)); then echo 1; else echo 0; fi "
let%test _ = fail_if_stmt_p "if ; then echo 1; else echo 0; fi"
let%test _ = fail_if_stmt_p "if ((1)); then ; else echo 0; fi"
let%test _ = fail_if_stmt_p "if ((1)); then echo 1; else ; fi"
let%test _ = fail_if_stmt_p "if ((1)); echo 1; else echo 0; fi"
let%test _ = fail_if_stmt_p "if ((1)); then echo 1; echo 0; fi"
let%test _ = fail_if_stmt_p "if ((1)); then echo 1; else echo 0"

(* -------------------- Case -------------------- *)

let succ_case_stmt_p = succ_p pp_case_stmt case_stmt_p
let fail_case_stmt_p = fail_p pp_case_stmt case_stmt_p

let%test _ =
  succ_case_stmt_p
    "case abc in ( *.txt | abc ) meow ;; ( *.ml ) woof ;; esac"
    (CaseStmt
       ( Word "abc"
       , [ CaseItem
             ( Word "*.txt"
             , [ Word "abc" ]
             , SinglePipeline
                 (Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), []))
             )
         ; CaseItem
             ( Word "*.ml"
             , []
             , SinglePipeline
                 (Pipeline (false, SimpleCommand (Command ([], Word "woof", []), []), []))
             )
         ] ))
;;

let%test _ =
  succ_case_stmt_p
    "case abc in\n\n( *.txt | abc ) meow ;;\n( *.ml ) woof ;;\nesac"
    (CaseStmt
       ( Word "abc"
       , [ CaseItem
             ( Word "*.txt"
             , [ Word "abc" ]
             , SinglePipeline
                 (Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), []))
             )
         ; CaseItem
             ( Word "*.ml"
             , []
             , SinglePipeline
                 (Pipeline (false, SimpleCommand (Command ([], Word "woof", []), []), []))
             )
         ] ))
;;

let%test _ = succ_case_stmt_p "case abc in esac" (CaseStmt (Word "abc", []))
let%test _ = fail_case_stmt_p " case abc in ( *.txt | abc ) meow ;; ( *.ml ) woof ;; esac"
let%test _ = fail_case_stmt_p "case abc in ( *.txt | abc ) meow ;; ( *.ml ) woof ;; esac "

let%test _ =
  fail_case_stmt_p " case abc in ( *.txt | abc ) meow ;; ( *.ml ) woof ;; esac "
;;

let%test _ = fail_case_stmt_p "case in ( *.txt | abc ) meow ;; ( *.ml ) woof ;; esac"
let%test _ = fail_case_stmt_p "case abc ( *.txt | abc ) meow ;; ( *.ml ) woof ;; esac"
let%test _ = fail_case_stmt_p "case abc in ( *.txt | abc ) meow ;; ( *.ml ) woof ;;"

(* -------------------- Case item -------------------- *)

let succ_case_item_p = succ_p pp_case_item case_item_p
let fail_case_item_p = fail_p pp_case_item case_item_p

let%test _ =
  succ_case_item_p
    "( *.txt | abc ) meow ;;"
    (CaseItem
       ( Word "*.txt"
       , [ Word "abc" ]
       , SinglePipeline
           (Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), [])) ))
;;

let%test _ =
  succ_case_item_p
    "*.txt|abc)meow;;"
    (CaseItem
       ( Word "*.txt"
       , [ Word "abc" ]
       , SinglePipeline
           (Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), [])) ))
;;

let%test _ =
  succ_case_item_p
    "*.txt)echo 1;;"
    (CaseItem
       ( Word "*.txt"
       , []
       , SinglePipeline
           (Pipeline
              (false, SimpleCommand (Command ([], Word "echo", [ Word "1" ]), []), [])) ))
;;

let%test _ = fail_case_item_p " ( *.txt | abc ) meow ;;"
let%test _ = fail_case_item_p "( *.txt | abc ) meow ;; "
let%test _ = fail_case_item_p " ( *.txt | abc ) meow ;; "
let%test _ = fail_case_item_p "( *.txt | abc meow ;;"
let%test _ = fail_case_item_p ") meow ;;"
let%test _ = fail_case_item_p "( *.txt | abc ) meow ;"
let%test _ = fail_case_item_p "( *.txt | abc ) ;;"

(* -------------------- Function -------------------- *)

let succ_func_p = succ_p pp_func func_p
let fail_func_p = fail_p pp_func func_p

let%test _ =
  succ_func_p
    "function meow_f () meow"
    (Func (Name "meow_f", SimpleCommand (Command ([], Word "meow", []), [])))
;;

let%test _ =
  succ_func_p
    "function meow_f meow"
    (Func (Name "meow_f", SimpleCommand (Command ([], Word "meow", []), [])))
;;

let%test _ =
  succ_func_p
    "meow_f() meow"
    (Func (Name "meow_f", SimpleCommand (Command ([], Word "meow", []), [])))
;;

let%test _ =
  succ_func_p
    "meow_f() meow >> a.txt"
    (Func
       ( Name "meow_f"
       , SimpleCommand (Command ([], Word "meow", []), [ AppendOtp (1, Word "a.txt") ]) ))
;;

let%test _ = fail_func_p " function meow_f () meow"
let%test _ = fail_func_p "function meow_f () meow "
let%test _ = fail_func_p " function meow_f () meow "
let%test _ = fail_func_p " meow_f () meow"
let%test _ = fail_func_p "meow_f () meow "
let%test _ = fail_func_p " meow_f () meow "
let%test _ = fail_func_p "meow_f meow"
let%test _ = fail_func_p "function () meow"
let%test _ = fail_func_p "function() meow"

(* -------------------- Script element -------------------- *)

let succ_script_elem_p = succ_p pp_script_elem script_elem_p
let fail_script_elem_p = fail_p pp_script_elem script_elem_p

let%test _ =
  succ_script_elem_p
    "echo 1"
    (Pipelines
       (SinglePipeline
          (Pipeline
             (false, SimpleCommand (Command ([], Word "echo", [ Word "1" ]), []), []))))
;;

let%test _ =
  succ_script_elem_p
    "meow_f() meow"
    (FuncDecl (Func (Name "meow_f", SimpleCommand (Command ([], Word "meow", []), []))))
;;

let%test _ = fail_script_elem_p " echo 1"
let%test _ = fail_script_elem_p "echo 1 "
let%test _ = fail_script_elem_p " echo 1 "
let%test _ = fail_script_elem_p " meow_f() meow"
let%test _ = fail_script_elem_p "meow_f() meow "
let%test _ = fail_script_elem_p " meow_f() meow "

(* -------------------- Script -------------------- *)

let succ_script_p = succ_p pp_script script_p
let fail_script_p = fail_p pp_script script_p

let%test _ =
  succ_script_p
    "echo 1"
    (Script
       [ Pipelines
           (SinglePipeline
              (Pipeline
                 (false, SimpleCommand (Command ([], Word "echo", [ Word "1" ]), []), [])))
       ])
;;

let%test _ =
  succ_script_p
    "meow_f() meow"
    (Script
       [ FuncDecl
           (Func (Name "meow_f", SimpleCommand (Command ([], Word "meow", []), [])))
       ])
;;

let%test _ =
  succ_script_p
    "meow_f() meow\necho 1"
    (Script
       [ FuncDecl
           (Func (Name "meow_f", SimpleCommand (Command ([], Word "meow", []), [])))
       ; Pipelines
           (SinglePipeline
              (Pipeline
                 (false, SimpleCommand (Command ([], Word "echo", [ Word "1" ]), []), [])))
       ])
;;

let%test _ =
  succ_script_p
    "    \n  \n\n\n meow_f() meow\n  \n  \n\n\n echo 1\n\n\n\n \n \n"
    (Script
       [ FuncDecl
           (Func (Name "meow_f", SimpleCommand (Command ([], Word "meow", []), [])))
       ; Pipelines
           (SinglePipeline
              (Pipeline
                 (false, SimpleCommand (Command ([], Word "echo", [ Word "1" ]), []), [])))
       ])
;;
