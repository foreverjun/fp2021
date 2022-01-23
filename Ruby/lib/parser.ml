open Ast
open Angstrom

(* Checkers *)
let is_whitespace = function ' ' | '\t' -> true | _ -> false
let is_stmt_sep = function '\n' | ';' | ' ' | '\t' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let is_sign = function '-' -> true | _ -> false
let is_dot = function '.' -> true | _ -> false
let is_quote = function '\"' -> true | _ -> false

let is_allowed_first_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' | '$' | '@' | '_' -> true
  | _ -> false

let is_variable = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

let is_reserved = function
  | "and" | "or" | "true" | "false" | "return" | "if" | "end" | "else" | "while"
  | "def" | "class" | "lambda" | "break" | "next" | "nil" ->
      true
  | _ -> false

(* Void Slaughterers *)
let skip_whitespace = skip_while is_whitespace
let skip_stmt_sep = skip_while is_stmt_sep
let token t = skip_whitespace *> string t

(* Brackets & Bars *)
let between t1 t2 e1 = t1 *> skip_whitespace *> e1 <* skip_whitespace <* t2
let round_brackets e1 = between (string "(") (string ")") e1
let curly_brackets e1 = between (string "{") (string "}") e1
let square_brackets e1 = between (string "[") (string "]") e1
let vertical_bars e1 = between (string "|") (string "|") e1

(* Takers *)
let take_number = take_while is_digit
let take_sign = take_while is_sign
let take_dot = take_while1 is_dot
let take_string = take_till is_quote
let take_variable = take_while is_variable

(* Expression & Statement & Identifier builders *)
let e_add e1 e2 = Add (e1, e2)
let e_sub e1 e2 = Sub (e1, e2)
let e_mul e1 e2 = Mul (e1, e2)
let e_div e1 e2 = Div (e1, e2)
let e_mod e1 e2 = Mod (e1, e2)
let e_eq e1 e2 = Equal (e1, e2)
let e_gr e1 e2 = Greater (e1, e2)
let e_gr_eq e1 e2 = GreaterOrEq (e1, e2)
let e_less e1 e2 = Less (e1, e2)
let e_less_eq e1 e2 = LessOrEq (e1, e2)
let e_and e1 e2 = And (e1, e2)
let e_or e1 e2 = Or (e1, e2)
let e_list el1 = List el1
let e_lambda el1 sl1 = Lambda (el1, sl1)
let e_func_call i1 i2 el1 = Call (i1, i2, el1)
let e_nil = Nil
let e_true = Constant (Boolean true)
let e_false = Constant (Boolean false)
let s_break = Break
let s_next = Next
let s_assign e1 e2 = Assign (e1, e2)
let s_expression e1 = Expression e1
let s_return e1 = Return e1
let s_if_else e1 sl1 sl2 = IfElse (e1, sl1, sl2)
let s_while e1 sl1 = While (e1, sl1)
let s_class i1 sl1 = Class (i1, sl1)
let s_func i1 el1 sl1 = Function (i1, el1, sl1)
let i_identifier i1 = Identifier i1

(* Lifters *)
let lift_return = lift s_return
let lift_class = lift2 s_class
let lift_lambda = lift2 e_lambda
let lift_assign = lift2 s_assign
let lift_while = lift2 s_while
let lift_func = lift3 s_func
let lift_func_call = lift3 e_func_call
let lift_if_else = lift3 s_if_else

(* Tokens *)
let t_comma = token ","
let t_quote = token "\""
let t_assign = token "="
let t_end = token "end"
let t_if = token "if"
let t_else = token "else"
let t_while = token "while"
let t_def = token "def"
let t_class = token "class"
let t_lambda = token "lambda"
let t_return = token "return"
let t_nil = token "nil"
let t_instance_v = token "@"
let t_global_v = token "$"
let t_class_v = token "@@"
let t_break = token "break"
let t_next = token "next"
let t_true = token "true"
let t_false = token "false"
let t_add = token "+"
let t_sub = token "-"
let t_mul = token "*"
let t_div = token "/"
let t_mod = token "%"
let t_eq = token "=="
let t_gr = token ">"
let t_gr_eq = token ">="
let t_less = token "<"
let t_less_eq = token "<="
let t_and = token "and"
let t_and_1 = token "&&"
let t_or = token "or"
let t_or_1 = token "||"

(* Chains *)
let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init

(* Single Parsers *)
let p_add = t_add *> return e_add
let p_sub = t_sub *> return e_sub
let p_mul = t_mul *> return e_mul
let p_div = t_div *> return e_div
let p_mod = t_mod *> return e_mod
let p_eq = t_eq *> return e_eq
let p_gr = t_gr *> return e_gr
let p_gr_eq = t_gr_eq *> return e_gr_eq
let p_less = t_less *> return e_less
let p_less_eq = t_less_eq *> return e_less_eq
let p_and = t_and *> return e_and
let p_and_1 = t_and_1 *> return e_and
let p_or = t_or *> return e_or
let p_or_1 = t_or_1 *> return e_or
let p_nil = t_nil *> return e_nil
let p_break = t_break *> return s_break
let p_next = t_next *> return s_next
let p_true = t_true *> return e_true
let p_false = t_false *> return e_false

let p_identifier =
  take_variable >>= function
  | x when not (is_reserved x) -> return @@ Identifier x
  | _ -> fail "variables with reserved words are prohibited"

let p_local_variable =
  take_variable >>= function
  | x when not (is_reserved x) -> return @@ Variable (Local, Identifier x)
  | _ -> fail "variables with reserved words are prohibited"

let p_instance_variable =
  t_instance_v *> take_variable >>= function
  | x when not (is_reserved x) -> return @@ Variable (Instance, Identifier x)
  | _ -> fail "variables with reserved words are prohibited"

let p_global_variable =
  t_global_v *> take_variable >>= function
  | x when not (is_reserved x) -> return @@ Variable (Global, Identifier x)
  | _ -> fail "variables with reserved words are prohibited"

let p_class_variable =
  t_class_v *> take_variable >>= function
  | x when not (is_reserved x) -> return @@ Variable (Class, Identifier x)
  | _ -> fail "variables with reserved words are prohibited"

let p_integer =
  take_sign >>= fun sign ->
  take_number >>= fun x ->
  return @@ Constant (Integer (int_of_string (sign ^ x)))

let p_float =
  take_sign >>= fun sign ->
  take_number >>= fun x ->
  take_dot >>= fun dot ->
  take_number >>= fun part ->
  return @@ Constant (Float (float_of_string (((sign ^ x) ^ dot) ^ part)))

let p_string =
  t_quote *> take_string <* t_quote >>= fun x -> return @@ Constant (String x)

let p_lambda el1 sl1 =
  t_lambda *> skip_whitespace
  *> curly_brackets
       (lift_lambda (vertical_bars el1) (sl1 <* skip_stmt_sep <|> return []))

let p_func_call el1 =
  lift_func_call p_identifier (take_dot *> p_identifier) (round_brackets el1)
  <|> lift_func_call (return Null) p_identifier (round_brackets el1)

let p_func el1 sl1 =
  let i = t_def *> skip_whitespace *> p_identifier <* skip_stmt_sep in
  lift_func i
    (round_brackets el1 <* skip_stmt_sep <|> return [])
    (sl1 <* skip_stmt_sep <* t_end)

let p_class sl1 =
  let i = t_class *> skip_whitespace *> p_identifier <* skip_stmt_sep in
  lift_class i sl1 <* skip_stmt_sep <* t_end

let p_while el1 sl1 =
  let e = t_while *> skip_whitespace *> el1 <* skip_stmt_sep in
  lift_while e sl1 <* skip_stmt_sep <* t_end

let p_if_else el1 sl1 =
  let e = t_if *> skip_whitespace *> el1 <* skip_stmt_sep in
  lift_if_else e (sl1 <* skip_stmt_sep)
    (t_else *> sl1 <* skip_stmt_sep <|> return [] <* t_end)

let p_return el1 =
  let e = t_return *> skip_whitespace *> (el1 <|> return e_nil) in
  lift_return e

let p_assign el1 = lift_assign (el1 <* t_assign) el1

(* Grouped Parsers *)
let gp_low_pr_op = p_add <|> p_sub
let gp_high_pr_op = p_mul <|> p_div <|> p_mod
let gp_compare_op = p_eq <|> p_gr_eq <|> p_less_eq <|> p_gr <|> p_less
let gp_logic_op = p_and <|> p_and_1 <|> p_or <|> p_or_1
let gp_number = p_float <|> p_integer
let gp_pseudo = p_true <|> p_false <|> p_nil
let gp_loop_jumps = p_break <|> p_next

let gp_variable =
  p_class_variable <|> p_global_variable <|> p_instance_variable
  <|> p_local_variable

(* Big Things Doers *)
type dispatch = {
  p_expression : dispatch -> expression t;
  p_statement : dispatch -> statement t;
}

let bundle =
  let p_expression duo =
    fix @@ fun p_expression ->
    let statement_list = sep_by skip_stmt_sep (duo.p_statement duo) in
    let expression_list = sep_by t_comma p_expression in
    let pe_picker =
      skip_whitespace *> peek_char_fail >>= function
      | x when is_allowed_first_letter x ->
          p_func_call expression_list
          <|> p_lambda expression_list statement_list
          <|> gp_variable <|> gp_pseudo
      | x when is_digit x || is_sign x -> gp_number
      | '\"' -> p_string
      | '(' -> round_brackets p_expression
      | '[' -> square_brackets expression_list >>| e_list
      | _ -> fail "Unsupported 〇 got in the way"
    in
    let pe_high_pr = chainl1 pe_picker gp_high_pr_op in
    let pe_low_pr = chainl1 pe_high_pr gp_low_pr_op in
    let pe_compare = chainl1 pe_low_pr gp_compare_op in
    chainl1 pe_compare gp_logic_op
  in
  let p_statement duo =
    fix @@ fun p_statement ->
    let expression_list = sep_by t_comma (duo.p_expression duo) in
    let statement_list = sep_by skip_stmt_sep p_statement in
    let ps_expression = duo.p_expression duo >>| s_expression in
    let ps_assign = p_assign (duo.p_expression duo) in
    let ps_return = p_return (duo.p_expression duo) in
    let ps_if_else = p_if_else (duo.p_expression duo) statement_list in
    let ps_while = p_while (duo.p_expression duo) statement_list in
    let ps_class = p_class statement_list in
    let ps_func = p_func expression_list statement_list in
    skip_stmt_sep
    *> (ps_assign <|> ps_return <|> gp_loop_jumps <|> ps_if_else <|> ps_while
      <|> ps_class <|> ps_func <|> ps_expression)
  in
  { p_expression; p_statement }

let p_final = sep_by skip_stmt_sep (bundle.p_statement bundle) <* skip_stmt_sep
let parse p s = parse_string ~consume:All p s
