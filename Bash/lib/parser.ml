open Angstrom
open Ast

(* -------------------- Common helper functions -------------------- *)

(** [chainl1 e op] parses one or more occurrences of [e], separated by [op].
Returns a value obtained by a left associative application of [op] to the values
returned by [e]. *)
let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

(** Integer parser *)
let int_p =
  option "" (string "+" <|> string "-")
  >>= fun sign ->
  take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
  >>| fun s -> int_of_string (sign ^ s)
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

let blank = take_while is_blank
let blank1 = take_while1 is_blank
let trim p = blank *> p <* blank
let parens p = char '(' *> trim p <* char ')'

let is_delim = function
  | '\n' | '\r' -> true
  | _ -> false
;;

let delim1 = take_while1 is_delim

let is_meta = function
  | '|' | '&' | ';' | '(' | ')' | '<' | '>' -> true
  | c when is_blank c || is_delim c -> true
  | _ -> false
;;

let meta = take_while is_meta
let non_meta = take_while1 (fun c -> not (is_meta c))

(* -------------------- Variables -------------------- *)

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
  take_while1 is_namechar
  >>= function
  | s when (not (is_name_beg s.[0])) || List.mem s reserved -> fail "Incorrect name"
  | s -> return s
;;

(** Variable parser *)
let var_p =
  name_p
  >>= fun n ->
  char '['
  *> take_while1 (function
         | ']' -> false
         | _ -> true)
  <* char ']'
  >>| (fun subscr -> Subscript (n, subscr))
  <|> return (SimpleVar n)
;;

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

(** Arithmetic parser *)
let arithm_p =
  let num = int_p >>| fun n -> Num n in
  let var = var_p >>| fun v -> Var v in
  fix (fun arithm_p ->
      let factor = parens arithm_p <|> num <|> var in
      let term = chainl1 factor (trim (mul <|> div)) in
      let expr = chainl1 term (trim (plus <|> minus)) in
      let comp = chainl1 expr (trim (lesseq <|> greatereq <|> less <|> greater)) in
      chainl1 comp (trim (equal <|> nequal)))
;;

(* -------------------- Word, expansions and simple command -------------------- *)

(** Word parser. Parameters determine which expansions may be performed. *)
let rec word_p ?(brc = true) ?(prm = true) ?(cmd = true) ?(ari = true) ?(fln = true) () =
  let skip = fail "Expansion not requested" in
  (if brc then brace_exp else skip)
  <|> (if prm then param_exp_p >>| fun p -> ParamExp p else skip)
  <|> (if cmd then inn_cmd_subst () else skip)
  <|> (if ari then arithm_exp else skip)
  <|> (if fln then filename_exp else skip)
  <|> (non_meta
      >>= function
      | s when List.mem s reserved -> fail "Reserved string"
      | s -> return (Word s))

(** Brace expansion *)
and brace_exp =
  let prefix =
    take_till (function
        | '{' -> true
        | c when is_meta c -> true
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
          | c when is_meta c -> false
          | _ -> true)
    in
    sep_by1 (char ',') str
    >>= function
    | [ _ ] -> fail "Single string"
    | strs -> return strs
  in
  let postfix =
    take_till (function
        | c when is_meta c -> true
        | _ -> false)
  in
  option "" prefix
  >>= fun pre ->
  char '{' *> (seq <|> strs)
  <* char '}'
  >>= fun body ->
  option "" postfix
  >>| fun post -> BraceExp (List.map (fun s -> String.concat "" [ pre; s; post ]) body)

(** Parameter expansion parser *)
and param_exp_p =
  let is_end c = is_meta c || c = '}' in
  let param = var_p >>| fun v -> Param v in
  let pos_param =
    int_p
    >>= fun x ->
    if x >= 0 then return (PosParam x) else fail "Illegal positional parameter"
  in
  let length = char '#' *> var_p >>| fun v -> Length v in
  let substring =
    var_p
    >>= fun v ->
    char ':' *> int_p
    >>= fun off -> option 0 (char ':' *> int_p) >>| fun len -> Substring (v, off, len)
  in
  let cut d t = var_p >>= fun v -> string d *> take_till is_end >>| fun p -> t (v, p) in
  let cut_min_beg = cut "#" (fun (v, p) -> CutMinBeg (v, p)) in
  let cut_max_beg = cut "##" (fun (v, p) -> CutMaxBeg (v, p)) in
  let cut_min_end = cut "%" (fun (v, p) -> CutMinEnd (v, p)) in
  let cut_max_end = cut "%%" (fun (v, p) -> CutMaxEnd (v, p)) in
  let subst d t =
    var_p
    >>= fun v ->
    string d *> take_till (fun c -> is_end c || c = '/')
    >>= fun p -> option "" (char '/' *> take_till is_end) >>| fun s -> t (v, p, s)
  in
  let subst_one = subst "/" (fun (v, p, s) -> SubstOne (v, p, s)) in
  let subst_all = subst "//" (fun (v, p, s) -> SubstAll (v, p, s)) in
  let subst_beg = subst "/#" (fun (v, p, s) -> SubstBeg (v, p, s)) in
  let subst_end = subst "/%" (fun (v, p, s) -> SubstEnd (v, p, s)) in
  char '$'
  *> (param
     <|> pos_param
     <|> (char '{'
          *> (pos_param
             <|> length
             <|> substring
             <|> cut_max_beg
             <|> cut_min_beg
             <|> cut_max_end
             <|> cut_min_end
             <|> subst_all
             <|> subst_beg
             <|> subst_end
             <|> subst_one
             <|> param)
         <* char '}'))

(** Command substitution *)
and inn_cmd_subst () = char '$' *> parens (inn_cmd_p ()) >>| fun cmd -> CmdSubst cmd

(** Arithmetic expansion *)
and arithm_exp = string "$((" *> trim arithm_p <* string "))" >>| fun a -> ArithmExp a

(** Filename expansion *)
and filename_exp =
  let fn_char = function
    | '*' | '?' | '[' -> true
    | _ -> false
  in
  non_meta
  >>= function
  | w when Base.String.exists ~f:fn_char w -> return (FilenameExp w)
  | _ -> fail "Not a filename pattern"

(** Inner assignment parser to use for mutual recursion *)
and inn_assignt_p () =
  let word = word_p ~brc:false ~fln:false in
  var_p
  >>= fun v ->
  char '='
  *> (parens (sep_by blank (word ()))
     >>| (fun ws -> CompoundAssignt (v, ws))
     <|> (option None (word () >>| fun w -> Some w) >>| fun w -> SimpleAssignt (v, w)))

(** Inner simple command parser to use for mutual recursion *)
and inn_cmd_p () =
  let word = word_p in
  let blank_if_ne = function
    | _ :: _ -> blank
    | [] -> peek_string 0
  in
  sep_by blank (inn_assignt_p ())
  >>= fun assignts ->
  option [] (blank_if_ne assignts *> sep_by1 blank (word ()))
  >>= fun words ->
  match assignts, words with
  | _, hd :: tl -> return (Command (assignts, hd, tl))
  | hd :: tl, [] -> return (Assignt (hd, tl))
  | [], [] -> fail "Empty simple command"
;;

(** Assignment parser *)
let assignt_p = inn_assignt_p ()

(** Simple command parser *)
let cmd_p = inn_cmd_p ()

(* -------------------- Command list, pipeline and compounds -------------------- *)

(** Redirection parser *)
let redir_p =
  let word = word_p in
  let parse_by s d act =
    option d int_p >>= fun fd -> string s *> blank *> word () >>| act fd
  in
  parse_by ">>" 1 (fun fd w -> AppendOtp (fd, w))
  <|> parse_by "<&" 0 (fun fd w -> DuplInp (fd, w))
  <|> parse_by ">&" 1 (fun fd w -> DuplOtp (fd, w))
  <|> parse_by "<" 0 (fun fd w -> RedirInp (fd, w))
  <|> parse_by ">" 1 (fun fd w -> RedirOtp (fd, w))
;;

(** Helper functions to parse reserved words in the middle of compounds *)
let ctrl_m s =
  (string ";" <|> delim1) *> many (delim1 <|> blank1) *> string s <* many delim1
;;

(** Helper functions to parse reserved words in the end of compounds *)
let ctrl_e s = (string ";" <|> delim1) *> many (delim1 <|> blank1) *> string s

(** Inner pipeline list parser to use for mutual recursion *)
let rec inn_pipeline_list_p () =
  let parse_tail sep = blank *> string sep *> trim (inn_pipeline_list_p ()) in
  inn_pipeline_p ()
  >>= fun hd ->
  parse_tail "&&"
  >>| (fun tl -> PipelineAndList (hd, tl))
  <|> (parse_tail "||" >>| fun tl -> PipelineOrList (hd, tl))
  <|> return (Pipeline hd)

(** Inner pipeline parser to use for mutual recursion *)
and inn_pipeline_p () =
  option false (char '!' <* blank1 >>| fun _ -> true)
  >>= fun neg ->
  inn_compound_p ()
  >>= fun hd ->
  option [] (blank *> char '|' *> sep_by1 (char '|') (trim (inn_compound_p ())))
  >>| fun tl -> neg, hd, tl

(** Inner compound command parser to use for mutual recursion *)
and inn_compound_p () =
  let parse_by p act =
    p >>= fun c -> option [] (blank *> sep_by1 blank redir_p) >>| act c
  in
  parse_by (inn_while_loop_p ()) (fun c rs -> While (c, rs))
  <|> parse_by (inn_for_list_loop_p ()) (fun c rs -> ForList (c, rs))
  <|> parse_by (inn_for_expr_loop_p ()) (fun c rs -> ForExpr (c, rs))
  <|> parse_by (inn_if_stmt_p ()) (fun c rs -> If (c, rs))
  <|> parse_by (inn_case_stmt_p ()) (fun c rs -> Case (c, rs))
  <|> parse_by
        (string "((" *> trim arithm_p <* string "))")
        (fun c rs -> ArithmExpr (c, rs))
  <|> parse_by cmd_p (fun c rs -> SimpleCommand (c, rs))

(** Inner while loop parser to use for mutual recursion *)
and inn_while_loop_p () =
  string "while" *> trim (inn_pipeline_list_p ())
  >>= fun cnd ->
  ctrl_m "do" *> trim (inn_pipeline_list_p ()) <* ctrl_e "done" >>| fun act -> cnd, act

(** Helper function to parse for loops in two forms *)
and for_loop_with : 'a. 'a t -> ('a * pipeline_list) t =
 fun p ->
  string "for" *> trim p
  >>= fun cnd ->
  ctrl_m "do" *> trim (inn_pipeline_list_p ()) <* ctrl_e "done" >>| fun act -> cnd, act

(** Inner for loop (list form) parser to use for mutual recursion *)
and inn_for_list_loop_p () : for_list_loop t =
  let word = word_p in
  let list_cnd =
    name_p >>= fun n -> trim (string "in") *> sep_by blank (word ()) >>| fun ws -> n, ws
  in
  for_loop_with list_cnd >>| fun ((n, ws), act) -> n, ws, act

(** Inner for loop (expression form) parser to use for mutual recursion *)
and inn_for_expr_loop_p () : for_expr_loop t =
  let expr_cnd =
    let expr = trim (option (Num 1) arithm_p) in
    string "((" *> expr
    >>= fun e1 ->
    char ';' *> expr
    >>= fun e2 -> char ';' *> expr <* string "))" >>| fun e3 -> e1, e2, e3
  in
  for_loop_with expr_cnd >>| fun ((e1, e2, e3), act) -> e1, e2, e3, act

(** Inner if statement parser to use for mutual recursion *)
and inn_if_stmt_p () =
  string "if" *> trim (inn_pipeline_list_p ())
  >>= fun cnd ->
  ctrl_m "then" *> trim (inn_pipeline_list_p ())
  >>= fun thn ->
  option None (ctrl_m "else" *> trim (inn_pipeline_list_p ()) >>| fun els -> Some els)
  <* ctrl_e "fi"
  >>| fun els -> cnd, thn, els

(** Inner case statement parser to use for mutual recursion *)
and inn_case_stmt_p () =
  let word = word_p ~brc:false ~fln:false in
  let trimd p = trim (many delim1 *> p <* many delim1) in
  string "case" *> trimd (word ())
  <* string "in"
  >>= fun w ->
  many1 (trimd (inn_case_item_p ()))
  <|> trimd (return [])
  <* string "esac"
  >>| fun cs -> w, cs

(** Inner case statement item parser to use for mutual recursion *)
and inn_case_item_p () =
  let word = word_p ~brc:false ~fln:false in
  option ' ' (char '(') *> sep_by1 (char '|') (trim (word ()))
  <* char ')'
  >>= fun ptrns ->
  trim (inn_pipeline_list_p ())
  <* string ";;"
  >>= fun act ->
  match ptrns with
  | hd :: tl -> return (hd, tl, act)
  | _ -> fail "sep_by1 cannot return an empty list"
;;

(** Pipeline list parser *)
let pipeline_list_p = inn_pipeline_list_p ()

(** Pipeline parser *)
let pipeline_p = inn_pipeline_p ()

(** Compound parser *)
let compound_p = inn_compound_p ()

(** While loop parser *)
let while_loop_p = inn_while_loop_p ()

(** For loop (list form) parser *)
let for_list_loop_p = inn_for_list_loop_p ()

(** For loop (expr form) parser *)
let for_expr_loop_p = inn_for_expr_loop_p ()

(** If statement parser *)
let if_stmt_p = inn_if_stmt_p ()

(** Case statement parser *)
let case_stmt_p = inn_case_stmt_p ()

(** Case item parser *)
let case_item_p = inn_case_item_p ()

(* -------------------- Function -------------------- *)

(** Function parser *)
let func_p : func t =
  string "function" *> trim name_p
  <* option "" (string "()" <* blank)
  <|> (name_p <* trim (string "()"))
  <* many delim1
  >>= fun n -> blank *> compound_p >>| fun body -> n, body
;;

(* -------------------- Script -------------------- *)

(** Script element parser *)
let script_elem_p =
  func_p >>| (fun f -> Func f) <|> (pipeline_list_p >>| fun p -> Pipelines p)
;;

(** Bash script parser *)
let script_p =
  let gap = many (blank1 <|> delim1) in
  let gap1 = blank *> delim1 *> gap in
  let rec helper g =
    option
      Empty
      (g *> script_elem_p >>= fun hd -> helper gap1 >>| fun tl -> Script (hd, tl))
  in
  gap *> helper (return []) <* gap
;;

(* -------------------- Main parser function -------------------- *)

(** Parses the given string as a Bash script *)
let parse = parse_string ~consume:All script_p

(* ----------------------------------------------- *)
(* -------------------- Tests -------------------- *)
(* ----------------------------------------------- *)

(* -------------------- Helper functions -------------------- *)

(** Parse string s with parser p *)
let test_parse p = parse_string ~consume:All p

(** Check if parser p returns result res on string s *)
let succ_p pp p s exp =
  match test_parse p s with
  | Error e ->
    Printf.printf "Error: %s\n" e;
    false
  | Ok res when exp = res -> true
  | Ok res ->
    let open Format in
    print_string "\n-------------------- Input --------------------\n";
    print_string s;
    print_string "\n------------------- Expected ------------------\n";
    pp std_formatter exp;
    print_string "\n-------------------- Actual -------------------\n";
    pp std_formatter res;
    print_string "\n-----------------------------------------------\n";
    false
;;

(** Check if parser p fails on string s *)
let fail_p pp p s =
  match test_parse p s with
  | Error _ -> true
  | Ok res ->
    let open Format in
    print_string "\n-------------------- Input --------------------\n";
    print_string s;
    print_string "\n-------------------- Actual -------------------\n";
    pp std_formatter res;
    print_string "\n-----------------------------------------------\n";
    false
;;

(* -------------------- Variable -------------------- *)

let succ_var_p = succ_p pp_var var_p
let fail_var_p = fail_p pp_var var_p

let%test _ = succ_var_p "VAR" (SimpleVar "VAR")
let%test _ = succ_var_p "_var" (SimpleVar "_var")
let%test _ = succ_var_p "ARR[hi there]" (Subscript ("ARR", "hi there"))
let%test _ = succ_var_p "ARR[ ]" (Subscript ("ARR", " "))
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
    (Plus (Plus (Var (SimpleVar "x"), Var (SimpleVar "y")), Num 1))
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

let%test _ = succ_param_exp "$ABC" (Param (SimpleVar "ABC"))
let%test _ = succ_param_exp "$1" (PosParam 1)
let%test _ = succ_param_exp "${ABC}" (Param (SimpleVar "ABC"))
let%test _ = succ_param_exp "${1}" (PosParam 1)
let%test _ = succ_param_exp "${#ABC}" (Length (SimpleVar "ABC"))
let%test _ = succ_param_exp "${ABC:-20}" (Substring (SimpleVar "ABC", -20, 0))
let%test _ = succ_param_exp "${ABC:5:5}" (Substring (SimpleVar "ABC", 5, 5))
let%test _ = succ_param_exp "${ABC#*.ml}" (CutMinBeg (SimpleVar "ABC", "*.ml"))
let%test _ = succ_param_exp "${ABC##*.ml}" (CutMaxBeg (SimpleVar "ABC", "*.ml"))
let%test _ = succ_param_exp "${ABC%*.ml}" (CutMinEnd (SimpleVar "ABC", "*.ml"))
let%test _ = succ_param_exp "${ABC%%*.ml}" (CutMaxEnd (SimpleVar "ABC", "*.ml"))
let%test _ = succ_param_exp "${ABC/a}" (SubstOne (SimpleVar "ABC", "a", ""))
let%test _ = succ_param_exp "${ABC/a/b}" (SubstOne (SimpleVar "ABC", "a", "b"))
let%test _ = succ_param_exp "${ABC//a}" (SubstAll (SimpleVar "ABC", "a", ""))
let%test _ = succ_param_exp "${ABC//a/b}" (SubstAll (SimpleVar "ABC", "a", "b"))
let%test _ = succ_param_exp "${ABC/#a}" (SubstBeg (SimpleVar "ABC", "a", ""))
let%test _ = succ_param_exp "${ABC/#a/b}" (SubstBeg (SimpleVar "ABC", "a", "b"))
let%test _ = succ_param_exp "${ABC/%a}" (SubstEnd (SimpleVar "ABC", "a", ""))
let%test _ = succ_param_exp "${ABC/%a/b}" (SubstEnd (SimpleVar "ABC", "a", "b"))
let%test _ = fail_param_exp "$-1"
let%test _ = fail_param_exp " $ABC"
let%test _ = fail_param_exp "$ABC "
let%test _ = fail_param_exp " $ABC "

(* -------------------- Command substitution -------------------- *)

let succ_cmd_subst = succ_p pp_word (inn_cmd_subst ())
let fail_cmd_subst = fail_p pp_word (inn_cmd_subst ())

let%test _ =
  succ_cmd_subst
    "$(X=2)"
    (CmdSubst (Assignt (SimpleAssignt (SimpleVar "X", Some (Word "2")), [])))
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
let%test _ = succ_word_p "$A" (ParamExp (Param (SimpleVar "A")))

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
  succ_cmd_p "A=123" (Assignt (SimpleAssignt (SimpleVar "A", Some (Word "123")), []))
;;

let%test _ = succ_cmd_p "A=" (Assignt (SimpleAssignt (SimpleVar "A", None), []))

let%test _ =
  succ_cmd_p
    "A=123      B=567      _ckd24=df!5[]%$~7"
    (Assignt
       ( SimpleAssignt (SimpleVar "A", Some (Word "123"))
       , [ SimpleAssignt (SimpleVar "B", Some (Word "567"))
         ; SimpleAssignt (SimpleVar "_ckd24", Some (Word "df!5[]%$~7"))
         ] ))
;;

let%test _ = succ_cmd_p "1A=123" (Command ([], Word "1A=123", []))

let%test _ =
  succ_cmd_p
    "ARR[3]=123"
    (Assignt (SimpleAssignt (Subscript ("ARR", "3"), Some (Word "123")), []))
;;

let%test _ = succ_cmd_p "ARR=()" (Assignt (CompoundAssignt (SimpleVar "ARR", []), []))

let%test _ =
  succ_cmd_p
    "ARR=( 1   2  abc    )"
    (Assignt (CompoundAssignt (SimpleVar "ARR", [ Word "1"; Word "2"; Word "abc" ]), []))
;;

let%test _ =
  succ_cmd_p
    "ARR1=( 1   2  abc    )        ARR2=(bcd)"
    (Assignt
       ( CompoundAssignt (SimpleVar "ARR1", [ Word "1"; Word "2"; Word "abc" ])
       , [ CompoundAssignt (SimpleVar "ARR2", [ Word "bcd" ]) ] ))
;;

let%test _ =
  succ_cmd_p "cmd arg1 arg2" (Command ([], Word "cmd", [ Word "arg1"; Word "arg2" ]))
;;

let%test _ =
  succ_cmd_p
    "VAR1=123    VAR2=    cmd     arg1     arg2"
    (Command
       ( [ SimpleAssignt (SimpleVar "VAR1", Some (Word "123"))
         ; SimpleAssignt (SimpleVar "VAR2", None)
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
let%test _ = succ_redir_p "< $a" (RedirInp (0, ParamExp (Param (SimpleVar "a"))))

(* -------------------- Pipeline list -------------------- *)

let succ_pipeline_list_p = succ_p pp_pipeline_list pipeline_list_p
let fail_pipeline_list_p = fail_p pp_pipeline_list pipeline_list_p

let%test _ =
  succ_pipeline_list_p
    "echo 1"
    (Pipeline (false, SimpleCommand (Command ([], Word "echo", [ Word "1" ]), []), []))
;;

let%test _ =
  succ_pipeline_list_p
    "echo 1 && echo 2"
    (PipelineAndList
       ( (false, SimpleCommand (Command ([], Word "echo", [ Word "1" ]), []), [])
       , Pipeline (false, SimpleCommand (Command ([], Word "echo", [ Word "2" ]), []), [])
       ))
;;

let%test _ =
  succ_pipeline_list_p
    "! echo 1 && ! echo 2"
    (PipelineAndList
       ( (true, SimpleCommand (Command ([], Word "echo", [ Word "1" ]), []), [])
       , Pipeline (true, SimpleCommand (Command ([], Word "echo", [ Word "2" ]), []), [])
       ))
;;

let%test _ =
  succ_pipeline_list_p
    "echo 1 || echo 2"
    (PipelineOrList
       ( (false, SimpleCommand (Command ([], Word "echo", [ Word "1" ]), []), [])
       , Pipeline (false, SimpleCommand (Command ([], Word "echo", [ Word "2" ]), []), [])
       ))
;;

let%test _ =
  succ_pipeline_list_p
    "echo 1 && echo 2 || echo 3"
    (PipelineAndList
       ( (false, SimpleCommand (Command ([], Word "echo", [ Word "1" ]), []), [])
       , PipelineOrList
           ( (false, SimpleCommand (Command ([], Word "echo", [ Word "2" ]), []), [])
           , Pipeline
               (false, SimpleCommand (Command ([], Word "echo", [ Word "3" ]), []), []) )
       ))
;;

let%test _ =
  succ_pipeline_list_p
    "echo 1 || echo 2 && echo 3"
    (PipelineOrList
       ( (false, SimpleCommand (Command ([], Word "echo", [ Word "1" ]), []), [])
       , PipelineAndList
           ( (false, SimpleCommand (Command ([], Word "echo", [ Word "2" ]), []), [])
           , Pipeline
               (false, SimpleCommand (Command ([], Word "echo", [ Word "3" ]), []), []) )
       ))
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
    (false, SimpleCommand (Command ([], Word "echo", [ Word "1" ]), []), [])
;;

let%test _ =
  succ_pipeline_p
    "! echo 1"
    (true, SimpleCommand (Command ([], Word "echo", [ Word "1" ]), []), [])
;;

let%test _ =
  succ_pipeline_p
    "!echo 1"
    (false, SimpleCommand (Command ([], Word "!echo", [ Word "1" ]), []), [])
;;

let%test _ =
  succ_pipeline_p
    "echo 1 | grep 1"
    ( false
    , SimpleCommand (Command ([], Word "echo", [ Word "1" ]), [])
    , [ SimpleCommand (Command ([], Word "grep", [ Word "1" ]), []) ] )
;;

let%test _ =
  succ_pipeline_p
    "while a; do meow; done 2>& 1 | grep 1 >> a.txt"
    ( false
    , While
        ( ( Pipeline (false, SimpleCommand (Command ([], Word "a", []), []), [])
          , Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), []) )
        , [ DuplOtp (2, Word "1") ] )
    , [ SimpleCommand
          (Command ([], Word "grep", [ Word "1" ]), [ AppendOtp (1, Word "a.txt") ])
      ] )
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
       ( ( Pipeline (false, SimpleCommand (Command ([], Word "a", []), []), [])
         , Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), []) )
       , [] ))
;;

let%test _ =
  succ_compound_p
    "while a; do meow; done >> a.txt"
    (While
       ( ( Pipeline (false, SimpleCommand (Command ([], Word "a", []), []), [])
         , Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), []) )
       , [ AppendOtp (1, Word "a.txt") ] ))
;;

let%test _ =
  succ_compound_p
    "while a; do meow; done>>a.txt"
    (While
       ( ( Pipeline (false, SimpleCommand (Command ([], Word "a", []), []), [])
         , Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), []) )
       , [ AppendOtp (1, Word "a.txt") ] ))
;;

let%test _ =
  succ_compound_p
    "while a; do meow; done >> a.txt 2>& 1"
    (While
       ( ( Pipeline (false, SimpleCommand (Command ([], Word "a", []), []), [])
         , Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), []) )
       , [ AppendOtp (1, Word "a.txt"); DuplOtp (2, Word "1") ] ))
;;

let%test _ =
  succ_compound_p
    "for i in 1 2 34; do meow; done >> a.txt"
    (ForList
       ( ( "i"
         , [ Word "1"; Word "2"; Word "34" ]
         , Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), []) )
       , [ AppendOtp (1, Word "a.txt") ] ))
;;

let%test _ =
  succ_compound_p
    "if ((1)); then meow; fi >> a.txt"
    (If
       ( ( Pipeline (false, ArithmExpr (Num 1, []), [])
         , Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), [])
         , None )
       , [ AppendOtp (1, Word "a.txt") ] ))
;;

let%test _ =
  succ_compound_p
    "case abc in esac >> a.txt"
    (Case ((Word "abc", []), [ AppendOtp (1, Word "a.txt") ]))
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
    ( Pipeline (false, SimpleCommand (Command ([], Word "a", []), []), [])
    , Pipeline (false, SimpleCommand (Command ([], Word "echo", [ Word "a" ]), []), []) )
;;

let%test _ =
  succ_while_loop_p
    "while a \n do echo a \n done"
    ( Pipeline (false, SimpleCommand (Command ([], Word "a", []), []), [])
    , Pipeline (false, SimpleCommand (Command ([], Word "echo", [ Word "a" ]), []), []) )
;;

let%test _ =
  succ_while_loop_p
    "while a ;\n\n do echo a; done"
    ( Pipeline (false, SimpleCommand (Command ([], Word "a", []), []), [])
    , Pipeline (false, SimpleCommand (Command ([], Word "echo", [ Word "a" ]), []), []) )
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

(* -------------------- For (list form) -------------------- *)

let succ_for_list_loop_p = succ_p pp_for_list_loop for_list_loop_p
let fail_for_list_loop_p = fail_p pp_for_list_loop for_list_loop_p

let%test _ =
  succ_for_list_loop_p
    "for i in 1 2 34; do meow; done"
    ( "i"
    , [ Word "1"; Word "2"; Word "34" ]
    , Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), []) )
;;

let%test _ =
  succ_for_list_loop_p
    "for i in 1 2 34;\n\n do meow;\n done"
    ( "i"
    , [ Word "1"; Word "2"; Word "34" ]
    , Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), []) )
;;

let%test _ =
  succ_for_list_loop_p
    "for i in 1 2 34\ndo\nmeow\ndone"
    ( "i"
    , [ Word "1"; Word "2"; Word "34" ]
    , Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), []) )
;;

let%test _ =
  succ_for_list_loop_p
    "for i in; do meow; done"
    ("i", [], Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), []))
;;

let%test _ = fail_for_list_loop_p " for i in 1 2 34; do meow; done"
let%test _ = fail_for_list_loop_p "for i in 1 2 34; do meow; done "
let%test _ = fail_for_list_loop_p " for i in 1 2 34; do meow; done "
let%test _ = fail_for_list_loop_p "for  in 1 2 3; do meow; done"
let%test _ = fail_for_list_loop_p "for i in 1 2 3\n; do meow\n; done"
let%test _ = fail_for_list_loop_p "for i in 1 2 3; do meow done"
let%test _ = fail_for_list_loop_p "for i in 1 2 3; do meow;"
let%test _ = fail_for_list_loop_p "for i in 1 2 3 do meow; done"
let%test _ = fail_for_list_loop_p "for i in 1 2 3; meow; done"

(* -------------------- For (expression form) -------------------- *)

let succ_for_expr_loop_p = succ_p pp_for_expr_loop for_expr_loop_p
let fail_for_expr_loop_p = fail_p pp_for_expr_loop for_expr_loop_p

let%test _ =
  succ_for_expr_loop_p
    "for ((0; 0; 1 + 1)); do meow; done"
    ( Num 0
    , Num 0
    , Plus (Num 1, Num 1)
    , Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), []) )
;;

let%test _ =
  succ_for_expr_loop_p
    "for (( 0; 0; 1 + 1 )) ; do meow; done"
    ( Num 0
    , Num 0
    , Plus (Num 1, Num 1)
    , Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), []) )
;;

let%test _ =
  succ_for_expr_loop_p
    "for ((;;)) ; do meow; done"
    ( Num 1
    , Num 1
    , Num 1
    , Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), []) )
;;

let%test _ = fail_for_expr_loop_p " for ((0; 0; 1 + 1)); do meow; done"
let%test _ = fail_for_expr_loop_p "for ((0; 0; 1 + 1)); do meow; done "
let%test _ = fail_for_expr_loop_p " for ((0; 0; 1 + 1)); do meow; done "
let%test _ = fail_for_expr_loop_p "for (0; 0; 1 + 1)); do meow; done"
let%test _ = fail_for_expr_loop_p "for ((0; 0; 1 + 1); do meow; done"
let%test _ = fail_for_expr_loop_p "for ((0; 0; 1 + 1)) do meow; done"
let%test _ = fail_for_expr_loop_p "for ((0; 1 + 1)); do meow; done"
let%test _ = fail_for_expr_loop_p "for ((;;0; 1 + 1)); do meow; done"

(* -------------------- If -------------------- *)

let succ_if_stmt_p = succ_p pp_if_stmt if_stmt_p
let fail_if_stmt_p = fail_p pp_if_stmt if_stmt_p

let%test _ =
  succ_if_stmt_p
    "if ((1)); then echo 1; else echo 0; fi"
    ( Pipeline (false, ArithmExpr (Num 1, []), [])
    , Pipeline (false, SimpleCommand (Command ([], Word "echo", [ Word "1" ]), []), [])
    , Some
        (Pipeline (false, SimpleCommand (Command ([], Word "echo", [ Word "0" ]), []), []))
    )
;;

let%test _ =
  succ_if_stmt_p
    "if ((1))\nthen echo 1\nelse echo 0\nfi"
    ( Pipeline (false, ArithmExpr (Num 1, []), [])
    , Pipeline (false, SimpleCommand (Command ([], Word "echo", [ Word "1" ]), []), [])
    , Some
        (Pipeline (false, SimpleCommand (Command ([], Word "echo", [ Word "0" ]), []), []))
    )
;;

let%test _ =
  succ_if_stmt_p
    "if ((1)); then echo 1; fi"
    ( Pipeline (false, ArithmExpr (Num 1, []), [])
    , Pipeline (false, SimpleCommand (Command ([], Word "echo", [ Word "1" ]), []), [])
    , None )
;;

let%test _ =
  succ_if_stmt_p
    "if ((1)) ;\n\n then echo 1;\n fi"
    ( Pipeline (false, ArithmExpr (Num 1, []), [])
    , Pipeline (false, SimpleCommand (Command ([], Word "echo", [ Word "1" ]), []), [])
    , None )
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
    ( Word "abc"
    , [ ( Word "*.txt"
        , [ Word "abc" ]
        , Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), []) )
      ; ( Word "*.ml"
        , []
        , Pipeline (false, SimpleCommand (Command ([], Word "woof", []), []), []) )
      ] )
;;

let%test _ =
  succ_case_stmt_p
    "case abc in\n\n( *.txt | abc ) meow ;;\n( *.ml ) woof ;;\nesac"
    ( Word "abc"
    , [ ( Word "*.txt"
        , [ Word "abc" ]
        , Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), []) )
      ; ( Word "*.ml"
        , []
        , Pipeline (false, SimpleCommand (Command ([], Word "woof", []), []), []) )
      ] )
;;

let%test _ = succ_case_stmt_p "case abc in esac" (Word "abc", [])
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
    ( Word "*.txt"
    , [ Word "abc" ]
    , Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), []) )
;;

let%test _ =
  succ_case_item_p
    "*.txt|abc)meow;;"
    ( Word "*.txt"
    , [ Word "abc" ]
    , Pipeline (false, SimpleCommand (Command ([], Word "meow", []), []), []) )
;;

let%test _ =
  succ_case_item_p
    "*.txt)echo 1;;"
    ( Word "*.txt"
    , []
    , Pipeline (false, SimpleCommand (Command ([], Word "echo", [ Word "1" ]), []), []) )
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
    ("meow_f", SimpleCommand (Command ([], Word "meow", []), []))
;;

let%test _ =
  succ_func_p
    "function meow_f meow"
    ("meow_f", SimpleCommand (Command ([], Word "meow", []), []))
;;

let%test _ =
  succ_func_p "meow_f() meow" ("meow_f", SimpleCommand (Command ([], Word "meow", []), []))
;;

let%test _ =
  succ_func_p
    "meow_f() meow >> a.txt"
    ( "meow_f"
    , SimpleCommand (Command ([], Word "meow", []), [ AppendOtp (1, Word "a.txt") ]) )
;;

let%test _ =
  succ_func_p
    "meow_f()\nmeow >> a.txt"
    ( "meow_f"
    , SimpleCommand (Command ([], Word "meow", []), [ AppendOtp (1, Word "a.txt") ]) )
;;

let%test _ =
  succ_func_p
    "meow_f()\n   meow >> a.txt"
    ( "meow_f"
    , SimpleCommand (Command ([], Word "meow", []), [ AppendOtp (1, Word "a.txt") ]) )
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
       (Pipeline (false, SimpleCommand (Command ([], Word "echo", [ Word "1" ]), []), [])))
;;

let%test _ =
  succ_script_elem_p
    "meow_f() meow"
    (Func ("meow_f", SimpleCommand (Command ([], Word "meow", []), [])))
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
       ( Pipelines
           (Pipeline
              (false, SimpleCommand (Command ([], Word "echo", [ Word "1" ]), []), []))
       , Empty ))
;;

let%test _ =
  succ_script_p
    "meow_f() meow"
    (Script (Func ("meow_f", SimpleCommand (Command ([], Word "meow", []), [])), Empty))
;;

let%test _ =
  succ_script_p
    "meow_f() meow\necho 1"
    (Script
       ( Func ("meow_f", SimpleCommand (Command ([], Word "meow", []), []))
       , Script
           ( Pipelines
               (Pipeline
                  (false, SimpleCommand (Command ([], Word "echo", [ Word "1" ]), []), []))
           , Empty ) ))
;;

let%test _ =
  succ_script_p
    "    \n  \n\n\n meow_f() meow\n  \n  \n\n\n echo 1\n\n\n\n \n \n"
    (Script
       ( Func ("meow_f", SimpleCommand (Command ([], Word "meow", []), []))
       , Script
           ( Pipelines
               (Pipeline
                  (false, SimpleCommand (Command ([], Word "echo", [ Word "1" ]), []), []))
           , Empty ) ))
;;

let%test _ = succ_script_p "         \n\n \n" Empty
