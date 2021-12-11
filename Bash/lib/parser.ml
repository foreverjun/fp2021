open Angstrom
open Ast

(* -------------------- Interface -------------------- *)

let parse p s = parse_string ~consume:All p s

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
let trim p = blank *> p <* blank
let parens p = char '(' *> trim p <* char ')'

let is_delim = function
  | '\n' | '\r' | ';' -> true
  | _ -> false
;;

let delim = take_while is_delim

let is_meta = function
  | '|' | '&' | '(' | ')' | '<' | '>' -> true
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
  peek_char
  >>= function
  | Some c when is_name_beg c -> take_while is_namechar >>| fun s -> Name s
  | _ -> fail "Name should begin with a letter or underscore"
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

(* Operands *)
let num = int_p >>| fun n -> Num n

(** Arithmetic parser *)
let arithm_p =
  let var = var_p >>| fun v -> Var v in
  fix (fun arithm_p ->
      let factor = trim (parens arithm_p <|> num <|> var) in
      let term = chainl1 factor (trim (mul <|> div)) in
      let expr = chainl1 term (trim (plus <|> minus)) in
      let comp = chainl1 expr (trim (lesseq <|> greatereq <|> less <|> greater)) in
      chainl1 comp (trim (equal <|> nequal)))
;;

(* -------------------- Word, expansions and simple command -------------------- *)

(** Word parser. Parameters determine which expansions may be performed. *)
let rec word_p ?(brc = true) ?(prm = true) ?(cmd = true) ?(ari = true) () =
  let skip = fail "This expansion was not requested" in
  (if brc then brace_exp else skip)
  <|> (if prm then param_exp_p >>| fun p -> ParamExp p else skip)
  <|> (if cmd then inn_cmd_subst () else skip)
  <|> (if ari then arithm_exp else skip)
  <|> (non_meta >>| fun s -> Word s)

(* Brace expansion *)
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
  option "" postfix >>| fun post -> BraceExp (List.map (fun s -> pre ^ s ^ post) body)

(** Parameter expansion parser *)
and param_exp_p =
  let is_end c = is_meta c || c = '}' in
  let param = var_p >>| fun v -> Param v in
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
     <|> (char '{'
          *> (length
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

(* Command substitution *)
and inn_cmd_subst () = char '$' *> parens (inn_cmd_p ()) >>| fun cmd -> CmdSubst cmd

(* Arithmetic expansion *)
and arithm_exp = string "$((" *> arithm_p <* string "))" >>| fun a -> ArithmExp a

(* Inner assignment parser to use for mutual recursion *)
and inn_assignt_p () =
  let word = word_p ~brc:false in
  var_p
  >>= fun v ->
  char '='
  *> (parens (sep_by blank (word ()))
     >>| (fun ws -> CompoundAssignt (v, ws))
     <|> (option None (word () >>| fun w -> Some w) >>| fun w -> SimpleAssignt (v, w)))

(* Inner simple command parser to use for mutual recursion *)
and inn_cmd_p () =
  let word = word_p in
  sep_by blank (inn_assignt_p ())
  >>= fun assignts ->
  blank *> sep_by blank (word ())
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
  let parse_by s d act = option d int_p >>= fun fd -> string s *> word () >>| act fd in
  parse_by ">>" 1 (fun fd w -> Append_otp (fd, w))
  <|> parse_by "<&" 0 (fun fd w -> Dupl_inp (fd, w))
  <|> parse_by ">&" 1 (fun fd w -> Dupl_otp (fd, w))
  <|> parse_by "<" 0 (fun fd w -> Redir_inp (fd, w))
  <|> parse_by ">" 1 (fun fd w -> Redir_otp (fd, w))
;;

let ctrl s = list [ delim; blank; string s ]

(* Inner pipeline list parser to use for mutual recursion *)
let rec inn_pipeline_list_p () =
  let parse_tail sep = blank *> string sep *> trim (inn_pipeline_list_p ()) in
  inn_pipeline_p ()
  >>= fun hd ->
  parse_tail "&&"
  >>| (fun tl -> PipelineAndList (hd, tl))
  <|> (parse_tail "||" >>| fun tl -> PipelineOrList (hd, tl))
  <|> return (SinglePipeline hd)

(* Inner pipeline parser to use for mutual recursion *)
and inn_pipeline_p () =
  option false (char '!' >>| fun _ -> true)
  >>= fun neg ->
  sep_by1 (char '|') (trim (inn_compound_p ()))
  >>| function
  | hd :: tl -> Pipeline (neg, hd, tl)
  | _ -> failwith "sep_by1 cannot return an empty list"

(* Inner compound command parser to use for mutual recursion *)
and inn_compound_p () =
  let parse_by p act = p >>= fun c -> blank *> sep_by blank redir_p >>| act c in
  parse_by (inn_while_loop_p ()) (fun c rs -> While (c, rs))
  <|> parse_by (inn_for_loop_p ()) (fun c rs -> For (c, rs))
  <|> parse_by (inn_if_stmt_p ()) (fun c rs -> If (c, rs))
  <|> parse_by (inn_case_stmt_p ()) (fun c rs -> Case (c, rs))
  <|> parse_by
        (string "((" *> trim arithm_p <* string "))")
        (fun c rs -> ArithmExpr (c, rs))
  <|> parse_by cmd_p (fun c rs -> SimpleCommand (c, rs))

(* Inner while loop parser to use for mutual recursion *)
and inn_while_loop_p () =
  string "while" *> trim (inn_pipeline_list_p ())
  >>= fun cnd ->
  ctrl "do" *> trim (inn_pipeline_list_p ())
  <* ctrl "done"
  >>| fun act -> WhileLoop (cnd, act)

(* Inner for loop parser to use for mutual recursion *)
and inn_for_loop_p () =
  let word = word_p in
  let list_cnd =
    name_p >>= fun n -> trim (string "in") *> many (word ()) >>| fun ws -> n, ws
  in
  let expr_cnd =
    string "((" *> trim arithm_p
    >>= fun e1 ->
    char ';' *> trim arithm_p
    >>= fun e2 -> char ';' *> trim arithm_p <* string "))" >>| fun e3 -> e1, e2, e3
  in
  let parse_with p =
    string "for" *> trim p
    >>= fun cnd ->
    ctrl "do" *> trim (inn_pipeline_list_p ()) <* ctrl "done" >>| fun act -> cnd, act
  in
  parse_with list_cnd
  >>| (fun ((n, ws), act) -> ListFor (n, ws, act))
  <|> (parse_with expr_cnd >>| fun ((e1, e2, e3), act) -> ExprFor (e1, e2, e3, act))

(* Inner if statement parser to use for mutual recursion *)
and inn_if_stmt_p () =
  string "if" *> trim (inn_pipeline_list_p ())
  >>= fun cnd ->
  ctrl "then" *> trim (inn_pipeline_list_p ())
  >>= fun thn ->
  option None (ctrl "else" *> trim (inn_pipeline_list_p ()) >>| fun els -> Some els)
  <* ctrl "fi"
  >>| fun els -> IfStmt (cnd, thn, els)

(* Inner case statement parser to use for mutual recursion *)
and inn_case_stmt_p () =
  let word = word_p ~brc:false in
  string "case" *> trim (word ())
  <* string "in"
  >>= fun w ->
  trim (many (inn_case_item_p ())) <* string "esac" >>| fun cs -> CaseStmt (w, cs)

(* Inner case statement item parser to use for mutual recursion *)
and inn_case_item_p () =
  let word = word_p ~brc:false in
  option ' ' (char '(') *> sep_by1 (char '|') (trim (word ()))
  <* char ')'
  >>= fun ptrns ->
  trim (inn_pipeline_list_p ())
  <* string ";;"
  >>| fun act ->
  match ptrns with
  | hd :: tl -> CaseItem (hd, tl, act)
  | _ -> failwith "sep_by1 cannot return an empty list"
;;

(** Pipeline list parser *)
let pipeline_list_p = inn_pipeline_list_p ()

(** Pipeline parser *)
let pipeline_p = inn_pipeline_p ()

(** Compound parser *)
let compound_p = inn_compound_p ()

(** While loop parser *)
let while_loop_p = inn_while_loop_p ()

(** For loop parser *)
let for_loop_p = inn_for_loop_p ()

(** If statement parser *)
let if_stmt_p = inn_if_stmt_p ()

(** Case statement parser *)
let case_stmt_p = inn_case_stmt_p ()

(** Case item parser *)
let case_item_p = inn_case_item_p ()
