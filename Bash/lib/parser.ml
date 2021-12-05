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
let meta = take_while is_metachar
let non_meta = take_while1 (fun c -> not (is_metachar c))

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
let parens p = char '(' *> blank *> p <* blank <* char ')'
let num = int_p >>| fun n -> Num n

(** Arithmetic parser *)
let arithm_p =
  let var = var_p >>| fun v -> Var v in
  let trim p = blank *> p <* blank in
  fix (fun arithm_p ->
      let factor = trim (parens arithm_p <|> num <|> var) in
      let term = chainl1 factor (trim (mul <|> div)) in
      let expr = chainl1 term (trim (plus <|> minus)) in
      let comp = chainl1 expr (trim (lesseq <|> greatereq <|> less <|> greater)) in
      chainl1 comp (trim (equal <|> nequal)))
;;

(* -------------------- Expansions -------------------- *)

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

(* Parameter expansion *)

(** Parameter expansion parser *)
let param_exp_p =
  let is_end c = is_metachar c || c = '}' in
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
;;

(* Command substitution -- currently not implemented as it requires mutually recursive parsing *)
(* let cmd_subst = string "$(" *> cmd_p <* char ')' >>| fun cmd -> CmdSubst cmd *)

(* Arithmetic expansion *)
let arithm_exp = string "$((" *> arithm_p <* string "))" >>| fun a -> ArithmExp a

(* -------------------- Word with expansions -------------------- *)

(** Word parser  *)
let word_p =
  let str_word_p = non_meta >>| fun s -> Word s in
  param_exp_p >>| (fun p -> ParamExp p) <|> arithm_exp <|> str_word_p
;;

(* -------------------- Simple command -------------------- *)

(** Assignment parser *)
let assignt_p =
  var_p
  >>= fun v ->
  char '='
  *> (char '(' *> blank *> sep_by blank word_p
     <* blank
     <* char ')'
     >>| (fun ws -> CompoundAssignt (v, ws))
     <|> (option None (word_p >>| fun w -> Some w) >>| fun w -> SimpleAssignt (v, w)))
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
