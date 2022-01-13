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

(** [choice_sep_by1 sep p1 p2] parses one or more elements separated by [sep] using [p1]
or [p2] returning the results *)
let either_sep_by1 s p1 p2 =
  fix (fun m ->
      lift2
        (fun (e1, e2) (l1, l2) ->
          match e1, e2 with
          | Some e1, Some e2 -> e1 :: l1, e2 :: l2
          | Some e, None -> e :: l1, l2
          | None, Some e -> l1, e :: l2
          | None, None -> l1, l2)
        (p1 >>| (fun e -> Some e, None) <|> (p2 >>| fun e -> None, Some e))
        (s *> m <|> return ([], [])))
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
  ; "{"
  ; "}"
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

let delim = take_while is_delim
let delim1 = take_while1 is_delim
let sc_delim1 = string ";" <|> delim1

let is_meta = function
  | '|' | '&' | ';' | '(' | ')' | '<' | '>' -> true
  | c when is_blank c || is_delim c -> true
  | _ -> false
;;

let meta = take_while is_meta
let non_meta = take_while1 (fun c -> not (is_meta c))

(** Reserved words in the middle of compounds *)
let ctrl_m s = sc_delim1 *> many (delim1 <|> blank1) *> string s <* many delim1

(** Reserved words at the end of compounds *)
let ctrl_e s = sc_delim1 *> many (delim1 <|> blank1) *> string s

(* -------------------- Variables -------------------- *)

(** Name of a variable or a function *)
let name =
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
let var_p : var t =
  name
  >>= fun n ->
  option "0" (char '[' *> take_while1 (( <> ) ']') <* char ']') >>| fun i -> n, i
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
  fix (fun arithm ->
      let fctr = parens arithm <|> num <|> var in
      let term = chainl1 fctr (trim (mul <|> div)) in
      let expr = chainl1 term (trim (plus <|> minus)) in
      let comp = chainl1 expr (trim (lesseq <|> greatereq <|> less <|> greater)) in
      let equl = chainl1 comp (trim (equal <|> nequal)) in
      let asnt =
        var_p >>= fun x -> trim (char '=') *> equl >>| fun v -> ArithmAssignt (x, v)
      in
      asnt <|> equl)
;;

(* -------------------- Word and expansions -------------------- *)

(** Word parser. Parameters determine which expansions may be performed. *)
let rec word_p
    ?(dqu = true)
    ?(brc = true)
    ?(prm = true)
    ?(cmd = true)
    ?(ari = true)
    ?(fln = true)
    ()
  =
  fix (fun _ ->
      let skip = fail "Expansion not requested" in
      char '\'' *> take_till (( = ) '\'')
      <* char '\''
      >>| (fun s -> Word s)
      <|> (if dqu then inn_double_qoutes_p () else skip)
      <|> (if brc then brace_exp () else skip)
      <|> (if prm then param_exp_p >>| fun p -> ParamExp p else skip)
      <|> (if cmd then inn_cmd_subst () else skip)
      <|> (if ari then arithm_exp else skip)
      <|> (if fln then filename_exp else skip)
      <|> (non_meta
          >>= function
          | s when List.mem s reserved -> fail "Reserved string"
          | s -> return (Word s)))

(** Double quotes *)
and inn_double_qoutes_p () =
  let word = word_p ~dqu:false ~brc:false ~fln:false in
  let escape =
    char '\\' *> choice [ char '$'; char '`'; char '\"'; char '\\' ]
    >>| (fun c -> Word (String.make 1 c))
    <|> string "\\\n" *> return (Word "")
    <|> char '\\' *> return (Word "\\")
    <|> (peek_char_fail
        >>= function
        | '$' | '`' -> word ()
        | _ -> fail "Expected a quoted $ or `")
  in
  let non_escape =
    take_while1 (function
        | '\"' | '$' | '`' | '\\' -> false
        | _ -> true)
    >>| fun s -> Word s
  in
  let part = escape <|> non_escape in
  char '\"' *> many part <* char '\"' >>| fun ws -> DoubleQuotes ws

(** Brace expansion *)
and brace_exp () =
  let word = word_p ~brc:false in
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
          | s, e ->
            incr <* char '}' >>| range_by (fun c -> String.make 1 (Char.chr c)) s e)
    <|> (elems_by int_p
        >>= function
        | s, e -> incr <* char '}' >>| range_by (fun n -> string_of_int n) s e)
  in
  let strs =
    let str =
      take_while (function
          | ',' | '}' -> false
          | c when is_meta c -> false
          | _ -> true)
    in
    sep_by1 (char ',') str
    <* char '}'
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
  >>= fun body ->
  option "" postfix
  >>= fun post ->
  (* Constructing a string of form "pre+b1+post pre+b2+post ..." *)
  let s =
    List.fold_left
      (fun acc b -> String.concat " " [ acc; String.concat "" [ pre; b; post ] ])
      ""
      body
  in
  (* Parsing the resulting string as a list of words without brace expansions *)
  match parse_string ~consume:All (trim (sep_by1 blank (word ()))) s with
  | Ok ws -> return (BraceExp ws)
  | Error s -> fail s

(** Parameter expansion parser *)
and param_exp_p =
  let is_end c = is_meta c || c = '}' in
  let param = var_p >>| fun v -> Param v in
  let pos_param =
    int_p
    >>= fun x ->
    if x >= 0
    then return (Param (string_of_int x, "0"))
    else fail "Illegal positional parameter"
  in
  let length = char '#' *> var_p >>| fun v -> Length v in
  let substring =
    var_p
    >>= fun v ->
    char ':' *> trim arithm_p
    >>= fun off ->
    option None (char ':' *> trim arithm_p >>| fun n -> Some n)
    >>| fun len -> Substring (v, off, len)
  in
  let cut d t =
    var_p >>= fun v -> string d *> take_till is_end >>| fun p -> t (v, String.trim p)
  in
  let cut_min_beg = cut "#" (fun (v, p) -> CutMinBeg (v, p)) in
  let cut_max_beg = cut "##" (fun (v, p) -> CutMaxBeg (v, p)) in
  let cut_min_end = cut "%" (fun (v, p) -> CutMinEnd (v, p)) in
  let cut_max_end = cut "%%" (fun (v, p) -> CutMaxEnd (v, p)) in
  let subst d t =
    var_p
    >>= fun v ->
    string d *> take_till (fun c -> is_end c || c = '/')
    >>= fun p ->
    option "" (char '/' *> take_till is_end)
    >>| fun s -> t (v, String.trim p, String.trim s)
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
and inn_cmd_subst () =
  char '$' *> parens (inn_cmd_p ())
  <|> (char '`' *> inn_cmd_p () <* char '`')
  >>| fun cmd -> CmdSubst cmd

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

(* -------------------- Assignment -------------------- *)

(** Inner assignment parser to use for mutual recursion *)
and inn_assignt_p () =
  let word = word_p ~brc:false ~fln:false in
  let simple =
    var_p
    >>= fun v -> char '=' *> option (Word "") (word ()) >>| fun w -> SimpleAssignt (v, w)
  in
  let compound p c =
    name >>= fun n -> char '=' *> parens (sep_by1 blank (p ())) >>| fun vs -> c n vs
  in
  let ind = compound word (fun n ws -> IndArrAssignt (n, ws)) in
  let assoc =
    compound
      (fun () -> name <* char '=' >>= fun k -> word () >>| fun v -> k, v)
      (fun n ps -> AssocArrAssignt (n, ps))
  in
  assoc <|> ind <|> simple

(* -------------------- Redirection -------------------- *)

(** Inner redirection parser to use for mutual recursion *)
and inn_redir_p () =
  let word = word_p in
  let parse_by s d act =
    option d int_p >>= fun fd -> string s *> blank *> word () >>| act fd
  in
  parse_by ">>" 1 (fun fd w -> AppendOtp (fd, w))
  <|> parse_by "<&" 0 (fun fd w -> DuplInp (fd, w))
  <|> parse_by ">&" 1 (fun fd w -> DuplOtp (fd, w))
  <|> parse_by "<" 0 (fun fd w -> RedirInp (fd, w))
  <|> parse_by ">" 1 (fun fd w -> RedirOtp (fd, w))

(* -------------------- Compound command -------------------- *)

(** Inner compound command parser to use for mutual recursion *)
and inn_compound_p () =
  let td p =
    let d = blank1 <|> sc_delim1 in
    many d *> p <* many d
  in
  char '{' *> td (sep_by1 (many (blank1 <|> sc_delim1)) (inn_pipe_list_p ()))
  <* char '}'
  >>| (fun pl -> Group pl)
  <|> (inn_while_loop_p () >>| fun (cnd, act) -> While (cnd, act))
  <|> (inn_for_list_loop_p () >>| fun (n, ws, act) -> ForList (n, ws, act))
  <|> (inn_for_expr_loop_p () >>| fun (e1, e2, e3, act) -> ForExpr (e1, e2, e3, act))
  <|> (inn_if_stmt_p () >>| fun (cnd, thn, els) -> If (cnd, thn, els))
  <|> (inn_case_stmt_p () >>| fun (w, cs) -> Case (w, cs))
  <|> (string "((" *> td arithm_p <* string "))" >>| fun a -> ArithmExpr a)

(** Inner while loop parser to use for mutual recursion *)
and inn_while_loop_p () =
  string "while" *> trim (inn_pipe_list_p ())
  >>= fun cnd ->
  ctrl_m "do" *> trim (inn_pipe_list_p ()) <* ctrl_e "done" >>| fun act -> cnd, act

(** Helper function to parse for loops in two forms *)
and for_loop_with : 'a. 'a t -> ('a * pipe_list) t =
 fun p ->
  string "for" *> trim p
  >>= fun cnd ->
  ctrl_m "do" *> trim (inn_pipe_list_p ()) <* ctrl_e "done" >>| fun act -> cnd, act

(** Inner for loop (list form) parser to use for mutual recursion *)
and inn_for_list_loop_p () =
  let word = word_p in
  let list_cnd =
    name >>= fun n -> trim (string "in") *> sep_by blank (word ()) >>| fun ws -> n, ws
  in
  for_loop_with list_cnd >>| fun ((n, ws), act) -> n, ws, act

(** Inner for loop (expression form) parser to use for mutual recursion *)
and inn_for_expr_loop_p () =
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
  string "if" *> trim (inn_pipe_list_p ())
  >>= fun cnd ->
  ctrl_m "then" *> trim (inn_pipe_list_p ())
  >>= fun thn ->
  option None (ctrl_m "else" *> trim (inn_pipe_list_p ()) >>| fun els -> Some els)
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
  >>= fun ptrns -> trim (inn_pipe_list_p ()) <* string ";;" >>| fun act -> ptrns, act

(* -------------------- Command -------------------- *)

(** Inner simple command parser to use for mutual recursion *)
and inn_simple_p () =
  let word = word_p in
  let blank_if_ne = function
    | _ :: _ -> blank
    | [] -> peek_string 0
  in
  sep_by blank (inn_assignt_p ())
  >>= fun assignts ->
  option ([], []) (blank_if_ne assignts *> either_sep_by1 blank (inn_redir_p ()) (word ()))
  >>= fun (rs, ws) ->
  match assignts, ws with
  | _, _ :: _ | _ :: _, [] -> return (assignts, ws, rs)
  | [], [] -> fail "Empty simple command"

(** Inner command parser to use for mutual recursion *)
and inn_cmd_p () =
  inn_simple_p ()
  >>| (fun (assignts, ws, rs) -> Simple (assignts, ws, rs))
  <|> (inn_compound_p ()
      >>= fun c ->
      option [] (blank *> sep_by1 blank (inn_redir_p ())) >>| fun rs -> Compound (c, rs))

(* -------------------- Pipe and pipe list -------------------- *)

(** Inner pipe parser to use for mutual recursion *)
and inn_pipe_p () =
  option false (char '!' <* blank1 >>| fun _ -> true)
  >>= fun neg ->
  inn_cmd_p ()
  >>= fun hd ->
  option [] (blank *> char '|' *> sep_by1 (char '|') (trim (inn_cmd_p ())))
  >>| fun tl -> neg, hd, tl

(** Inner pipe list parser to use for mutual recursion *)
and inn_pipe_list_p () =
  let parse_tail sep = trim sep *> delim *> inn_pipe_list_p () in
  inn_pipe_p ()
  >>= fun hd ->
  parse_tail (string "&&")
  >>| (fun tl -> PipeAndList (hd, tl))
  <|> (parse_tail (string "||") >>| fun tl -> PipeOrList (hd, tl))
  <|> return (Pipe hd)
;;

(* -------------------- Mutually recursive parsers -------------------- *)

(** Assignment parser *)
let assignt_p = inn_assignt_p ()

(** Redirection parser *)
let redir_p = inn_redir_p ()

(** Simple command parser *)
let simple_p = inn_simple_p ()

(** Compound command *)
let compound_p = inn_compound_p ()

(** Command parser *)
let cmd_p = inn_cmd_p ()

(** Pipeline parser *)
let pipe_p = inn_pipe_p ()

(** Pipeline list parser *)
let pipe_list_p = inn_pipe_list_p ()

(* -------------------- Function -------------------- *)

(** Function parser *)
let func_p : func t =
  string "function" *> trim name
  <* option "" (string "()" <* blank)
  <|> (name <* trim (string "()"))
  <* many delim1
  >>= fun n ->
  blank *> compound_p
  >>= fun body -> option [] (blank *> sep_by1 blank redir_p) >>| fun rs -> n, body, rs
;;

(* -------------------- Script -------------------- *)

(** Comment (at the beginning of a line) parser *)
let comment = char '#' *> take_while (( <> ) '\n') <* char '\n'

(** Script element parser *)
let script_elem_p = func_p >>| (fun f -> Func f) <|> (pipe_list_p >>| fun ps -> Pipes ps)

(** Bash script parser *)
let script_p : script t =
  let gap = many (blank1 <|> delim1 <|> comment) in
  let gap1 = blank *> delim1 *> gap in
  gap *> sep_by gap1 script_elem_p <* gap
;;

(* -------------------- Main parser function -------------------- *)

(** Parses the given string as a Bash script returning a [result] *)
let parse_result = parse_string ~consume:All script_p

(** Creates and returns a parser [state] for parsing a Bash script *)
let make_state () = Buffered.parse script_p

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

let%test _ = succ_var_p "VAR" ("VAR", "0")
let%test _ = succ_var_p "_var" ("_var", "0")
let%test _ = succ_var_p "ARR[hi there]" ("ARR", "hi there")
let%test _ = succ_var_p "ARR[ ]" ("ARR", " ")
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
  succ_arithm_p "x + y + 1" (Plus (Plus (Var ("x", "0"), Var ("y", "0")), Num 1))
;;

let%test _ = succ_arithm_p "x = 1 + 2" (ArithmAssignt (("x", "0"), Plus (Num 1, Num 2)))

let%test _ =
  succ_arithm_p "( x=1 == 2)" (ArithmAssignt (("x", "0"), Equal (Num 1, Num 2)))
;;

let%test _ = fail_arithm_p " 100"
let%test _ = fail_arithm_p "100 "
let%test _ = fail_arithm_p " 100 "
let%test _ = fail_arithm_p "5 5"
let%test _ = fail_arithm_p "(()"
let%test _ = fail_arithm_p "+ -"
let%test _ = fail_arithm_p "123ab"
let%test _ = fail_arithm_p "2 + 2 == 4))"
let%test _ = fail_arithm_p "2=2"
let%test _ = fail_arithm_p "1 + x = 2"

(* -------------------- Brace expansion -------------------- *)

let succ_brace_exp = succ_p pp_word (brace_exp ())
let fail_brace_exp = fail_p pp_word (brace_exp ())

let%test _ =
  succ_brace_exp "ab{c,d,e}fd" (BraceExp [ Word "abcfd"; Word "abdfd"; Word "abefd" ])
;;

let%test _ = succ_brace_exp "ab{c,d,e}" (BraceExp [ Word "abc"; Word "abd"; Word "abe" ])
let%test _ = succ_brace_exp "{c,d,e}fd" (BraceExp [ Word "cfd"; Word "dfd"; Word "efd" ])

let%test _ =
  succ_brace_exp "ab{,,}fd" (BraceExp [ Word "abfd"; Word "abfd"; Word "abfd" ])
;;

let%test _ = fail_brace_exp "ab{}fd"
let%test _ = fail_brace_exp "ab{c}fd"

let%test _ =
  succ_brace_exp "1a{1..3}b5" (BraceExp [ Word "1a1b5"; Word "1a2b5"; Word "1a3b5" ])
;;

let%test _ = succ_brace_exp "a{1..-1}b" (BraceExp [ Word "a1b"; Word "a0b"; Word "a-1b" ])
let%test _ = succ_brace_exp "1a{1..1}b5" (BraceExp [ Word "1a1b5" ])
let%test _ = succ_brace_exp "1a{1..4..2}b5" (BraceExp [ Word "1a1b5"; Word "1a3b5" ])
let%test _ = succ_brace_exp "1a{1..4..-2}b5" (BraceExp [ Word "1a1b5"; Word "1a3b5" ])

let%test _ =
  succ_brace_exp
    "1a{1..4..0}b5"
    (BraceExp [ Word "1a1b5"; Word "1a2b5"; Word "1a3b5"; Word "1a4b5" ])
;;

let%test _ =
  succ_brace_exp "1a{3..1}b5" (BraceExp [ Word "1a3b5"; Word "1a2b5"; Word "1a1b5" ])
;;

let%test _ =
  succ_brace_exp
    "1a{-5..0..2}b5"
    (BraceExp [ Word "1a-5b5"; Word "1a-3b5"; Word "1a-1b5" ])
;;

let%test _ = succ_brace_exp "1a{d..a..2}b5" (BraceExp [ Word "1adb5"; Word "1abb5" ])
let%test _ = succ_brace_exp "/{lib,*}" (BraceExp [ Word "/lib"; FilenameExp "/*" ])
let%test _ = fail_brace_exp "$((1{-1..1}))"
let%test _ = fail_brace_exp " ab{c,d,e}fd"
let%test _ = fail_brace_exp "ab{c,d,e}fd "
let%test _ = fail_brace_exp " ab{c,d,e}fd "
let%test _ = fail_brace_exp "1a{d..a..}b5"

(* -------------------- Parameter expansion -------------------- *)

let succ_param_exp = succ_p pp_param_exp param_exp_p
let fail_param_exp = fail_p pp_param_exp param_exp_p

let%test _ = succ_param_exp "$ABC" (Param ("ABC", "0"))
let%test _ = succ_param_exp "$1" (Param ("1", "0"))
let%test _ = succ_param_exp "${ABC}" (Param ("ABC", "0"))
let%test _ = succ_param_exp "${1}" (Param ("1", "0"))
let%test _ = succ_param_exp "${#ABC}" (Length ("ABC", "0"))
let%test _ = succ_param_exp "${ABC:-20}" (Substring (("ABC", "0"), Num (-20), None))

let%test _ =
  succ_param_exp
    "${ABC:  5 + 5  :  5 }"
    (Substring (("ABC", "0"), Plus (Num 5, Num 5), Some (Num 5)))
;;

let%test _ = succ_param_exp "${ABC#*.ml}" (CutMinBeg (("ABC", "0"), "*.ml"))
let%test _ = succ_param_exp "${ABC##*.ml}" (CutMaxBeg (("ABC", "0"), "*.ml"))
let%test _ = succ_param_exp "${ABC%*.ml}" (CutMinEnd (("ABC", "0"), "*.ml"))
let%test _ = succ_param_exp "${ABC%%*.ml}" (CutMaxEnd (("ABC", "0"), "*.ml"))
let%test _ = succ_param_exp "${ABC/a}" (SubstOne (("ABC", "0"), "a", ""))
let%test _ = succ_param_exp "${ABC/a/b}" (SubstOne (("ABC", "0"), "a", "b"))
let%test _ = succ_param_exp "${ABC//a}" (SubstAll (("ABC", "0"), "a", ""))
let%test _ = succ_param_exp "${ABC//a/b}" (SubstAll (("ABC", "0"), "a", "b"))
let%test _ = succ_param_exp "${ABC/#a}" (SubstBeg (("ABC", "0"), "a", ""))
let%test _ = succ_param_exp "${ABC/#a/b}" (SubstBeg (("ABC", "0"), "a", "b"))
let%test _ = succ_param_exp "${ABC/%a}" (SubstEnd (("ABC", "0"), "a", ""))
let%test _ = succ_param_exp "${ABC/%a/b}" (SubstEnd (("ABC", "0"), "a", "b"))
let%test _ = fail_param_exp "$-1"
let%test _ = fail_param_exp "${1[0]}"
let%test _ = fail_param_exp " $ABC"
let%test _ = fail_param_exp "$ABC "
let%test _ = fail_param_exp " $ABC "

(* -------------------- Command substitution -------------------- *)

let succ_cmd_subst = succ_p pp_word (inn_cmd_subst ())
let fail_cmd_subst = fail_p pp_word (inn_cmd_subst ())

let%test _ =
  succ_cmd_subst
    "$(X=2)"
    (CmdSubst (Simple ([ SimpleAssignt (("X", "0"), Word "2") ], [], [])))
;;

let%test _ =
  succ_cmd_subst "$(echo hey)" (CmdSubst (Simple ([], [ Word "echo"; Word "hey" ], [])))
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

let succ_word_p ?(d = true) ?(b = true) ?(p = true) ?(c = true) ?(a = true) ?(f = true) =
  succ_p pp_word (word_p ~dqu:d ~brc:b ~prm:p ~cmd:c ~ari:a ~fln:f ())
;;

let fail_word_p ?(d = true) ?(b = true) ?(p = true) ?(c = true) ?(a = true) ?(f = true) =
  fail_p pp_word (word_p ~dqu:d ~brc:b ~prm:p ~cmd:c ~ari:a ~fln:f ())
;;

let%test _ = succ_word_p "something" (Word "something")
let%test _ = succ_word_p {|"something"|} (DoubleQuotes [ Word "something" ])
let%test _ = succ_word_p "1{a,b}2" (BraceExp [ Word "1a2"; Word "1b2" ])
let%test _ = succ_word_p {|"1{a,b}2"|} (DoubleQuotes [ Word "1{a,b}2" ])
let%test _ = succ_word_p "$A" (ParamExp (Param ("A", "0")))
let%test _ = succ_word_p {|"$A"|} (DoubleQuotes [ ParamExp (Param ("A", "0")) ])

let%test _ =
  succ_word_p "$(cmd arg)" (CmdSubst (Simple ([], [ Word "cmd"; Word "arg" ], [])))
;;

let%test _ =
  succ_word_p
    {|"$(cmd arg)"|}
    (DoubleQuotes [ CmdSubst (Simple ([], [ Word "cmd"; Word "arg" ], [])) ])
;;

let%test _ = succ_word_p "$((3 / 1))" (ArithmExp (Div (Num 3, Num 1)))

let%test _ =
  succ_word_p {|"$((3 / 1))"|} (DoubleQuotes [ ArithmExp (Div (Num 3, Num 1)) ])
;;

let%test _ = succ_word_p "?.a" (FilenameExp "?.a")
let%test _ = succ_word_p {|"?.a"|} (DoubleQuotes [ Word "?.a" ])
let%test _ = succ_word_p {|'"$A"'|} (Word {|"$A"|})
let%test _ = succ_word_p {|'1{a,b}2'|} (Word "1{a,b}2")
let%test _ = succ_word_p {|'$A'|} (Word "$A")
let%test _ = succ_word_p {|'$(cmd arg)'|} (Word "$(cmd arg)")
let%test _ = succ_word_p {|'$((3 / 1))'|} (Word "$((3 / 1))")
let%test _ = succ_word_p {|'?.a'|} (Word "?.a")
let%test _ = succ_word_p {|"\$A"|} (DoubleQuotes [ Word "$"; Word "A" ])
let%test _ = succ_word_p {|"\$(cmd arg)"|} (DoubleQuotes [ Word "$"; Word "(cmd arg)" ])
let%test _ = succ_word_p {|"\$((3 / 1))"|} (DoubleQuotes [ Word "$"; Word "((3 / 1))" ])

let%test _ =
  succ_word_p
    {|"abc$(echo)$((3 / 1))"|}
    (DoubleQuotes
       [ Word "abc"
       ; CmdSubst (Simple ([], [ Word "echo" ], []))
       ; ArithmExp (Div (Num 3, Num 1))
       ])
;;

let%test _ = fail_word_p " something"
let%test _ = fail_word_p "something "
let%test _ = fail_word_p " something "
let%test _ = fail_word_p "if"
let%test _ = succ_word_p ~b:false "1{a,b}2" (Word "1{a,b}2")
let%test _ = succ_word_p ~f:false "?.a" (Word "?.a")

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
let%test _ = succ_redir_p "< $a" (RedirInp (0, ParamExp (Param ("a", "0"))))

(* -------------------- Assignment -------------------- *)

let succ_assignt_p = succ_p pp_assignt assignt_p
let fail_assignt_p = fail_p pp_assignt assignt_p

let%test _ = succ_assignt_p "A=123" (SimpleAssignt (("A", "0"), Word "123"))
let%test _ = succ_assignt_p "A=" (SimpleAssignt (("A", "0"), Word ""))
let%test _ = fail_assignt_p "1A=123"
let%test _ = succ_assignt_p "ARR[3]=123" (SimpleAssignt (("ARR", "3"), Word "123"))

let%test _ =
  succ_assignt_p
    "ARR=( 1   2  abc    )"
    (IndArrAssignt ("ARR", [ Word "1"; Word "2"; Word "abc" ]))
;;

let%test _ = fail_assignt_p "ARR=()"
let%test _ = fail_assignt_p "ARR[1]=(a b c)"

let%test _ =
  succ_assignt_p
    "ARR=(k1=v1 k2=v2)"
    (AssocArrAssignt ("ARR", [ "k1", Word "v1"; "k2", Word "v2" ]))
;;

let%test _ = fail_assignt_p "ARR[1]=(k1=v1 k2=v2)"
let%test _ = fail_assignt_p " A=123"
let%test _ = fail_assignt_p "A=123 "
let%test _ = fail_assignt_p " A=123 "

(* -------------------- Compound command -------------------- *)

let succ_compound_p = succ_p pp_compound compound_p
let fail_compound_p = fail_p pp_compound compound_p

let%test _ =
  succ_compound_p
    "{\n\n \n meow1\n    meow2;meow3\n  }"
    (Group
       [ Pipe (false, Simple ([], [ Word "meow1" ], []), [])
       ; Pipe (false, Simple ([], [ Word "meow2" ], []), [])
       ; Pipe (false, Simple ([], [ Word "meow3" ], []), [])
       ])
;;

let%test _ = fail_compound_p "{}"
let%test _ = fail_compound_p "{\n \n \n }"

let%test _ =
  succ_compound_p
    "while a; do meow; done"
    (While
       ( Pipe (false, Simple ([], [ Word "a" ], []), [])
       , Pipe (false, Simple ([], [ Word "meow" ], []), []) ))
;;

let%test _ =
  succ_compound_p
    "for i in 1 2 34; do meow; done"
    (ForList
       ( "i"
       , [ Word "1"; Word "2"; Word "34" ]
       , Pipe (false, Simple ([], [ Word "meow" ], []), []) ))
;;

let%test _ =
  succ_compound_p
    "if ((\n1\n)); then meow; fi"
    (If
       ( Pipe (false, Compound (ArithmExpr (Num 1), []), [])
       , Pipe (false, Simple ([], [ Word "meow" ], []), [])
       , None ))
;;

let%test _ = succ_compound_p "case abc in esac" (Case (Word "abc", []))
let%test _ = fail_compound_p " { meow }"
let%test _ = fail_compound_p "{ meow } "
let%test _ = fail_compound_p " { meow } "
let%test _ = fail_compound_p " while a; do meow; done"
let%test _ = fail_compound_p "while a; do meow; done "
let%test _ = fail_compound_p " while a; do meow; done "

(* -------------------- While -------------------- *)

let succ_while_loop_p = succ_p pp_compound compound_p
let fail_while_loop_p = fail_p pp_compound compound_p

let%test _ =
  succ_while_loop_p
    "while a; do echo a; done"
    (While
       ( Pipe (false, Simple ([], [ Word "a" ], []), [])
       , Pipe (false, Simple ([], [ Word "echo"; Word "a" ], []), []) ))
;;

let%test _ =
  succ_while_loop_p
    "while a \n do echo a \n done"
    (While
       ( Pipe (false, Simple ([], [ Word "a" ], []), [])
       , Pipe (false, Simple ([], [ Word "echo"; Word "a" ], []), []) ))
;;

let%test _ =
  succ_while_loop_p
    "while a ;\n\n do echo a; done"
    (While
       ( Pipe (false, Simple ([], [ Word "a" ], []), [])
       , Pipe (false, Simple ([], [ Word "echo"; Word "a" ], []), []) ))
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

let succ_for_list_loop_p = succ_p pp_compound compound_p
let fail_for_list_loop_p = fail_p pp_compound compound_p

let%test _ =
  succ_for_list_loop_p
    "for i in 1 2 34; do meow; done"
    (ForList
       ( "i"
       , [ Word "1"; Word "2"; Word "34" ]
       , Pipe (false, Simple ([], [ Word "meow" ], []), []) ))
;;

let%test _ =
  succ_for_list_loop_p
    "for i in 1 2 34;\n\n do meow;\n done"
    (ForList
       ( "i"
       , [ Word "1"; Word "2"; Word "34" ]
       , Pipe (false, Simple ([], [ Word "meow" ], []), []) ))
;;

let%test _ =
  succ_for_list_loop_p
    "for i in 1 2 34\ndo\nmeow\ndone"
    (ForList
       ( "i"
       , [ Word "1"; Word "2"; Word "34" ]
       , Pipe (false, Simple ([], [ Word "meow" ], []), []) ))
;;

let%test _ =
  succ_for_list_loop_p
    "for i in; do meow; done"
    (ForList ("i", [], Pipe (false, Simple ([], [ Word "meow" ], []), [])))
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

let succ_for_expr_loop_p = succ_p pp_compound compound_p
let fail_for_expr_loop_p = fail_p pp_compound compound_p

let%test _ =
  succ_for_expr_loop_p
    "for ((0; 0; 1 + 1)); do meow; done"
    (ForExpr
       ( Num 0
       , Num 0
       , Plus (Num 1, Num 1)
       , Pipe (false, Simple ([], [ Word "meow" ], []), []) ))
;;

let%test _ =
  succ_for_expr_loop_p
    "for (( 0; 0; 1 + 1 )) ; do meow; done"
    (ForExpr
       ( Num 0
       , Num 0
       , Plus (Num 1, Num 1)
       , Pipe (false, Simple ([], [ Word "meow" ], []), []) ))
;;

let%test _ =
  succ_for_expr_loop_p
    "for ((;;)) ; do meow; done"
    (ForExpr (Num 1, Num 1, Num 1, Pipe (false, Simple ([], [ Word "meow" ], []), [])))
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

let succ_if_stmt_p = succ_p pp_compound compound_p
let fail_if_stmt_p = fail_p pp_compound compound_p

let%test _ =
  succ_if_stmt_p
    "if ((1)); then echo 1; else echo 0; fi"
    (If
       ( Pipe (false, Compound (ArithmExpr (Num 1), []), [])
       , Pipe (false, Simple ([], [ Word "echo"; Word "1" ], []), [])
       , Some (Pipe (false, Simple ([], [ Word "echo"; Word "0" ], []), [])) ))
;;

let%test _ =
  succ_if_stmt_p
    "if ((1))\nthen echo 1\nelse echo 0\nfi"
    (If
       ( Pipe (false, Compound (ArithmExpr (Num 1), []), [])
       , Pipe (false, Simple ([], [ Word "echo"; Word "1" ], []), [])
       , Some (Pipe (false, Simple ([], [ Word "echo"; Word "0" ], []), [])) ))
;;

let%test _ =
  succ_if_stmt_p
    "if ((1)); then echo 1; fi"
    (If
       ( Pipe (false, Compound (ArithmExpr (Num 1), []), [])
       , Pipe (false, Simple ([], [ Word "echo"; Word "1" ], []), [])
       , None ))
;;

let%test _ =
  succ_if_stmt_p
    "if ((1)) ;\n\n then echo 1;\n fi"
    (If
       ( Pipe (false, Compound (ArithmExpr (Num 1), []), [])
       , Pipe (false, Simple ([], [ Word "echo"; Word "1" ], []), [])
       , None ))
;;

let%test _ = fail_if_stmt_p " if ((1)); then echo 1; else echo 0; fi"
let%test _ = fail_if_stmt_p "if ((1)); then echo 1; else echo 0; fi "
let%test _ = fail_if_stmt_p " if ((1)); then echo 1; else echo 0; fi "
let%test _ = fail_if_stmt_p "if ; then echo 1; else echo 0; fi"
let%test _ = fail_if_stmt_p "if ((1)); then ; else echo 0; fi"
let%test _ = fail_if_stmt_p "if ((1)); then echo 1; else ; fi"
let%test _ = fail_if_stmt_p "if ((1)); echo 1; else echo 0; fi"
let%test _ = fail_if_stmt_p "if ((1)); then echo 1; else echo 0"

(* -------------------- Case -------------------- *)

let succ_case_stmt_p = succ_p pp_compound compound_p
let fail_case_stmt_p = fail_p pp_compound compound_p

let%test _ =
  succ_case_stmt_p
    "case abc in ( *.txt | abc ) meow ;; esac"
    (Case
       ( Word "abc"
       , [ [ Word "*.txt"; Word "abc" ], Pipe (false, Simple ([], [ Word "meow" ], []), [])
         ] ))
;;

let%test _ =
  succ_case_stmt_p
    "case abc in *.txt | abc ) meow ;; esac"
    (Case
       ( Word "abc"
       , [ [ Word "*.txt"; Word "abc" ], Pipe (false, Simple ([], [ Word "meow" ], []), [])
         ] ))
;;

let%test _ =
  succ_case_stmt_p
    "case abc in *.txt | abc) meow ;; esac"
    (Case
       ( Word "abc"
       , [ [ Word "*.txt"; Word "abc" ], Pipe (false, Simple ([], [ Word "meow" ], []), [])
         ] ))
;;

let%test _ =
  succ_case_stmt_p
    "case abc in *.txt | abc) echo 1 ;; esac"
    (Case
       ( Word "abc"
       , [ ( [ Word "*.txt"; Word "abc" ]
           , Pipe (false, Simple ([], [ Word "echo"; Word "1" ], []), []) )
         ] ))
;;

let%test _ =
  succ_case_stmt_p
    "case abc in ( *.txt | abc ) meow ;; ( *.ml ) woof ;; esac"
    (Case
       ( Word "abc"
       , [ [ Word "*.txt"; Word "abc" ], Pipe (false, Simple ([], [ Word "meow" ], []), [])
         ; [ Word "*.ml" ], Pipe (false, Simple ([], [ Word "woof" ], []), [])
         ] ))
;;

let%test _ =
  succ_case_stmt_p
    "case abc in\n\n( *.txt | abc ) meow ;;\n( *.ml ) woof ;;\nesac"
    (Case
       ( Word "abc"
       , [ [ Word "*.txt"; Word "abc" ], Pipe (false, Simple ([], [ Word "meow" ], []), [])
         ; [ Word "*.ml" ], Pipe (false, Simple ([], [ Word "woof" ], []), [])
         ] ))
;;

let%test _ = succ_case_stmt_p "case abc in esac" (Case (Word "abc", []))
let%test _ = fail_case_stmt_p " case abc in ( *.txt | abc ) meow ;; ( *.ml ) woof ;; esac"

let%test _ =
  fail_case_stmt_p " case abc in ( *.txt | abc ) meow ;; ( *.ml ) woof ;; esac "
;;

let%test _ = fail_case_stmt_p "case abc in ( *.txt | abc ) meow ;; ( *.ml ) woof ;; esac "
let%test _ = fail_case_stmt_p "case abc in ( *.txt | abc meow ;; esac"
let%test _ = fail_case_stmt_p "case abc in ) meow ;; esac"
let%test _ = fail_case_stmt_p "case abc in ( *.txt | abc ) meow ; esac"
let%test _ = fail_case_stmt_p "case abc in ( *.txt | abc ) ;; esac"
let%test _ = fail_case_stmt_p "case in ( *.txt | abc ) meow ;; ( *.ml ) woof ;; esac"
let%test _ = fail_case_stmt_p "case abc ( *.txt | abc ) meow ;; ( *.ml ) woof ;; esac"
let%test _ = fail_case_stmt_p "case abc in ( *.txt | abc ) meow ;; ( *.ml ) woof ;;"

(* -------------------- Command -------------------- *)

let succ_cmd_p = succ_p pp_cmd cmd_p
let fail_cmd_p = fail_p pp_cmd cmd_p

let%test _ =
  succ_cmd_p
    "A=123      B=567      _ckd24=df!5[]%$~7"
    (Simple
       ( [ SimpleAssignt (("A", "0"), Word "123")
         ; SimpleAssignt (("B", "0"), Word "567")
         ; SimpleAssignt (("_ckd24", "0"), Word "df!5[]%$~7")
         ]
       , []
       , [] ))
;;

let%test _ = succ_cmd_p "1A=123" (Simple ([], [ Word "1A=123" ], []))

let%test _ =
  succ_cmd_p
    "ARR1=( 1   2  abc    )        ARR2=(bcd)"
    (Simple
       ( [ IndArrAssignt ("ARR1", [ Word "1"; Word "2"; Word "abc" ])
         ; IndArrAssignt ("ARR2", [ Word "bcd" ])
         ]
       , []
       , [] ))
;;

let%test _ =
  succ_cmd_p "cmd arg1 arg2" (Simple ([], [ Word "cmd"; Word "arg1"; Word "arg2" ], []))
;;

let%test _ =
  succ_cmd_p
    "VAR1=123    VAR2=    cmd     arg1     arg2"
    (Simple
       ( [ SimpleAssignt (("VAR1", "0"), Word "123")
         ; SimpleAssignt (("VAR2", "0"), Word "")
         ]
       , [ Word "cmd"; Word "arg1"; Word "arg2" ]
       , [] ))
;;

let%test _ =
  succ_cmd_p
    "echo 1 10>abc.txt"
    (Simple ([], [ Word "echo"; Word "1" ], [ RedirOtp (10, Word "abc.txt") ]))
;;

let%test _ =
  succ_cmd_p
    "for ((0; 0; 0)); do meow; done 99<&99"
    (Compound
       ( ForExpr (Num 0, Num 0, Num 0, Pipe (false, Simple ([], [ Word "meow" ], []), []))
       , [ DuplInp (99, Word "99") ] ))
;;

let%test _ = fail_cmd_p " echo 1"
let%test _ = fail_cmd_p "echo 1 "
let%test _ = fail_cmd_p " echo 1 "
let%test _ = fail_cmd_p "i=1 for ((; i < 3; i++)); do echo $i; done"

(* -------------------- Pipeline -------------------- *)

let succ_pipe_p = succ_p pp_pipe pipe_p
let fail_pipe_p = fail_p pp_pipe pipe_p

let%test _ = succ_pipe_p "echo 1" (false, Simple ([], [ Word "echo"; Word "1" ], []), [])
let%test _ = succ_pipe_p "! echo 1" (true, Simple ([], [ Word "echo"; Word "1" ], []), [])
let%test _ = succ_pipe_p "!echo 1" (false, Simple ([], [ Word "!echo"; Word "1" ], []), [])

let%test _ =
  succ_pipe_p
    "echo 1 | grep 1"
    ( false
    , Simple ([], [ Word "echo"; Word "1" ], [])
    , [ Simple ([], [ Word "grep"; Word "1" ], []) ] )
;;

let%test _ =
  succ_pipe_p
    "while a; do meow; done 2>& 1 | grep 1 >> a.txt"
    ( false
    , Compound
        ( While
            ( Pipe (false, Simple ([], [ Word "a" ], []), [])
            , Pipe (false, Simple ([], [ Word "meow" ], []), []) )
        , [ DuplOtp (2, Word "1") ] )
    , [ Simple ([], [ Word "grep"; Word "1" ], [ AppendOtp (1, Word "a.txt") ]) ] )
;;

let%test _ = fail_pipe_p " echo 1"
let%test _ = fail_pipe_p "echo 1 "
let%test _ = fail_pipe_p " echo 1 "

(* -------------------- Pipeline list -------------------- *)

let succ_pipe_list_p = succ_p pp_pipe_list pipe_list_p
let fail_pipe_list_p = fail_p pp_pipe_list pipe_list_p

let%test _ =
  succ_pipe_list_p "echo 1" (Pipe (false, Simple ([], [ Word "echo"; Word "1" ], []), []))
;;

let%test _ =
  succ_pipe_list_p
    "echo 1 && echo 2"
    (PipeAndList
       ( (false, Simple ([], [ Word "echo"; Word "1" ], []), [])
       , Pipe (false, Simple ([], [ Word "echo"; Word "2" ], []), []) ))
;;

let%test _ =
  succ_pipe_list_p
    "! echo 1 && ! echo 2"
    (PipeAndList
       ( (true, Simple ([], [ Word "echo"; Word "1" ], []), [])
       , Pipe (true, Simple ([], [ Word "echo"; Word "2" ], []), []) ))
;;

let%test _ =
  succ_pipe_list_p
    "echo 1 || echo 2"
    (PipeOrList
       ( (false, Simple ([], [ Word "echo"; Word "1" ], []), [])
       , Pipe (false, Simple ([], [ Word "echo"; Word "2" ], []), []) ))
;;

let%test _ =
  succ_pipe_list_p
    "echo 1 && echo 2 || echo 3"
    (PipeAndList
       ( (false, Simple ([], [ Word "echo"; Word "1" ], []), [])
       , PipeOrList
           ( (false, Simple ([], [ Word "echo"; Word "2" ], []), [])
           , Pipe (false, Simple ([], [ Word "echo"; Word "3" ], []), []) ) ))
;;

let%test _ =
  succ_pipe_list_p
    "echo 1 || echo 2 && echo 3"
    (PipeOrList
       ( (false, Simple ([], [ Word "echo"; Word "1" ], []), [])
       , PipeAndList
           ( (false, Simple ([], [ Word "echo"; Word "2" ], []), [])
           , Pipe (false, Simple ([], [ Word "echo"; Word "3" ], []), []) ) ))
;;

let%test _ = fail_pipe_list_p " echo 1"
let%test _ = fail_pipe_list_p "echo 1 "
let%test _ = fail_pipe_list_p " echo 1 "
let%test _ = fail_pipe_list_p "echo 1 && echo 2 "

(* -------------------- Function -------------------- *)

let succ_func_p = succ_p pp_func func_p
let fail_func_p = fail_p pp_func func_p

let%test _ =
  succ_func_p
    "function meow_f () { meow }"
    ("meow_f", Group [ Pipe (false, Simple ([], [ Word "meow" ], []), []) ], [])
;;

let%test _ =
  succ_func_p
    "function meow_f { meow }"
    ("meow_f", Group [ Pipe (false, Simple ([], [ Word "meow" ], []), []) ], [])
;;

let%test _ =
  succ_func_p
    "meow_f() { meow }"
    ("meow_f", Group [ Pipe (false, Simple ([], [ Word "meow" ], []), []) ], [])
;;

let%test _ =
  succ_func_p
    "meow_f() { meow } >> a.txt"
    ( "meow_f"
    , Group [ Pipe (false, Simple ([], [ Word "meow" ], []), []) ]
    , [ AppendOtp (1, Word "a.txt") ] )
;;

let%test _ =
  succ_func_p
    "meow_f()\n{ meow } >> a.txt"
    ( "meow_f"
    , Group [ Pipe (false, Simple ([], [ Word "meow" ], []), []) ]
    , [ AppendOtp (1, Word "a.txt") ] )
;;

let%test _ =
  succ_func_p
    "meow_f()\n  { meow } >> a.txt"
    ( "meow_f"
    , Group [ Pipe (false, Simple ([], [ Word "meow" ], []), []) ]
    , [ AppendOtp (1, Word "a.txt") ] )
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
    (Pipes (Pipe (false, Simple ([], [ Word "echo"; Word "1" ], []), [])))
;;

let%test _ =
  succ_script_elem_p
    "echo 1 | cat"
    (Pipes
       (Pipe
          ( false
          , Simple ([], [ Word "echo"; Word "1" ], [])
          , [ Simple ([], [ Word "cat" ], []) ] )))
;;

let%test _ =
  succ_script_elem_p
    "for i in 1 2 3; do\n  echo $i\ndone | grep 2"
    (Pipes
       (Pipe
          ( false
          , Compound
              ( ForList
                  ( "i"
                  , [ Word "1"; Word "2"; Word "3" ]
                  , Pipe
                      ( false
                      , Simple ([], [ Word "echo"; ParamExp (Param ("i", "0")) ], [])
                      , [] ) )
              , [] )
          , [ Simple ([], [ Word "grep"; Word "2" ], []) ] )))
;;

let%test _ =
  succ_script_elem_p
    "meow_f() { meow }"
    (Func ("meow_f", Group [ Pipe (false, Simple ([], [ Word "meow" ], []), []) ], []))
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
    [ Pipes (Pipe (false, Simple ([], [ Word "echo"; Word "1" ], []), [])) ]
;;

let%test _ =
  succ_script_p
    "meow_f() { meow }"
    [ Func ("meow_f", Group [ Pipe (false, Simple ([], [ Word "meow" ], []), []) ], []) ]
;;

let%test _ =
  succ_script_p
    {|
meow_f () {
  A=1
  echo meow
}
|}
    [ Func
        ( "meow_f"
        , Group
            [ Pipe (false, Simple ([ SimpleAssignt (("A", "0"), Word "1") ], [], []), [])
            ; Pipe (false, Simple ([], [ Word "echo"; Word "meow" ], []), [])
            ]
        , [] )
    ]
;;

let%test _ =
  succ_script_p
    {|
#!/bin/sh
echo meow
# some comment
#and another #one
|}
    [ Pipes (Pipe (false, Simple ([], [ Word "echo"; Word "meow" ], []), [])) ]
;;

let%test _ =
  succ_script_p
    "meow_f() {\n meow; }\necho 1 | cat"
    [ Func ("meow_f", Group [ Pipe (false, Simple ([], [ Word "meow" ], []), []) ], [])
    ; Pipes
        (Pipe
           ( false
           , Simple ([], [ Word "echo"; Word "1" ], [])
           , [ Simple ([], [ Word "cat" ], []) ] ))
    ]
;;

let%test _ =
  succ_script_p
    "    \n  \n\n\n meow_f() { meow }\n  \n  \n\n\n echo 1\n\n\n\n \n \n"
    [ Func ("meow_f", Group [ Pipe (false, Simple ([], [ Word "meow" ], []), []) ], [])
    ; Pipes (Pipe (false, Simple ([], [ Word "echo"; Word "1" ], []), []))
    ]
;;

let%test _ = succ_script_p "         \n\n \n" []
