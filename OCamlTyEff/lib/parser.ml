open Angstrom
open Ast

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a
let pspace = take_while is_space
let pspace1 = take_while1 is_space
let ptoken p = pspace *> p
let ptoken1 p = pspace1 *> p
let pstoken s = ptoken @@ string s
let pstoken1 s = ptoken1 @@ string s
let pparens p = pstoken "(" *> p <* pstoken ")"

let is_lower = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_upper = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_letter ch = is_lower ch || is_upper ch

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

(** @see <https://ocaml.org/manual/lex.html#sss:keywords> OCaml keywords *)
let is_keyword = function
  | "and"
  | "as"
  | "assert"
  | "asr"
  | "begin"
  | "class"
  | "constraint"
  | "do"
  | "done"
  | "downto"
  | "else"
  | "end"
  | "exception"
  | "external"
  | "false"
  | "for"
  | "fun"
  | "function"
  | "functor"
  | "if"
  | "in"
  | "include"
  | "inherit"
  | "initializer"
  | "land"
  | "lazy"
  | "let"
  | "lor"
  | "lsl"
  | "lsr"
  | "lxor"
  | "match"
  | "method"
  | "mod"
  | "module"
  | "mutable"
  | "new"
  | "nonrec"
  | "object"
  | "of"
  | "open"
  | "or"
  | "private"
  | "rec"
  | "sig"
  | "struct"
  | "then"
  | "to"
  | "true"
  | "try"
  | "type"
  | "val"
  | "virtual"
  | "when"
  | "while"
  | "with" -> true
  | _ -> false
;;

let pid =
  ptoken
  @@ lift2
       (fun hd tl -> Char.escaped hd ^ tl)
       (satisfy (fun ch -> ch = '_' || is_lower ch))
       (take_while (fun ch -> ch = '_' || is_letter ch || is_digit ch))
  >>= fun s ->
  if is_keyword s
  then fail "keyword reserved"
  else if s = "_"
  then fail "wildcard `_` isn't supported"
  else return s
;;

let ptupple pelm = pparens @@ sep_by (pstoken ",") pelm
let pexc1 = pstoken "Exc1" *> return exc1
let pexc2 = pstoken "Exc2" *> return exc2
let pexc = pexc1 <|> pexc2

let psign =
  choice [ pstoken "+" *> return 1; pstoken "-" *> return (-1); pstoken "" *> return 1 ]
;;

let pcint =
  ptoken
  @@ lift2
       (fun sign v -> c_int (sign * v))
       psign
       (take_while is_digit
       >>= fun s ->
       match int_of_string_opt s with
       | Some x -> return x
       | None -> fail "invalid int")
;;

let pcstring =
  c_string <$> ptoken @@ (string "\"" *> take_while (fun ch -> ch != '"')) <* string "\""
;;

let pcbool = c_bool <$> (pstoken "true" *> return true <|> pstoken "false" *> return false)
let pcnil = pstoken "[]" *> return c_nil
let pconst = choice [ pcint; pcstring; pcbool; pcnil ]
let ppval = p_val <$> pid
let ppconst = p_const <$> pconst

let ppatrn =
  fix
  @@ fun ppatrn ->
  let term = choice [ pparens ppatrn; ppconst; ppval ] in
  let term =
    lift2
      (fun ptrn ptrns ->
        match List.rev ptrns with
        | [] -> ptrn
        | hd :: tl -> p_cons (ptrn :: List.rev tl) hd)
      term
      (many (pstoken "::" *> term))
  in
  (fun l ->
    match l with
    | [ p ] -> p
    | _ -> p_tuple l)
  <$> sep_by1 (pstoken ",") term
  <|> pstoken "()" *> return p_unit
;;

let pdecl_base pexpr =
  pstoken "let"
  *> lift3
       (fun is_rec name expr -> { is_rec; name; expr })
       (pstoken1 "rec" *> return true <|> return false)
       (ptoken1 pid)
       (pstoken "=" *> pexpr)
;;

let pcase pkey pexpr = lift2 (fun k v -> k, v) (pstoken "|" *> pkey <* pstoken "->") pexpr
let peconst = e_const <$> pconst
let peval = e_val <$> pid
let pelet pexpr = lift2 e_let (pdecl_base pexpr) (pstoken1 "in" *> ptoken1 pexpr)

let pematch pexpr =
  lift2
    e_match
    (pstoken "match" *> ptoken1 pexpr <* pstoken1 "with")
    (many1 @@ pcase ppatrn pexpr)
;;

let pefun pexpr =
  pstoken "fun" *> lift2 (fun s e -> e_fun s e) (ptoken1 pid <* pstoken "->") pexpr
;;

let peapp pexpr = lift2 e_app pexpr (ptoken1 pexpr)

let petry pexpr =
  lift2
    e_try
    (pstoken "try" *> ptoken1 pexpr <* pstoken1 "with")
    (many1 @@ pcase pexc pexpr)
;;

let pebinop chain1 term binops =
  chain1
    term
    ((fun op expr1 expr2 -> e_app (e_app (e_val op) expr1) expr2)
    <$> choice (List.map pstoken binops))
;;

let pelbinop = pebinop chainl1
let perbinop = pebinop chainr1

let pexpr =
  fix (fun pexpr ->
      let term =
        choice [ pstoken "()" *> return e_unit; pparens pexpr; peconst; peval ]
      in
      let term =
        lift2
          (fun l expr -> List.fold_left (fun expr _ -> e_app (e_val "~!") expr) expr l)
          (many (pstoken "!"))
          term
      in
      let term =
        lift2 (fun expr l -> List.fold_left e_app expr l) term (many (ptoken1 term))
      in
      let term =
        lift2
          (fun l expr -> List.fold_left (fun expr _ -> e_app (e_val "~-") expr) expr l)
          (many (pstoken "-"))
          term
      in
      let term = pelbinop term [ "*"; "/" ] in
      let term = pelbinop term [ "+"; "-" ] in
      let term = perbinop term [ "::" ] in
      let term = pelbinop term [ "!="; "="; "<="; "<"; ">="; ">" ] in
      let term = perbinop term [ "&&" ] in
      let term = perbinop term [ "||" ] in
      let term =
        (fun l ->
          match l with
          | [ expr ] -> expr
          | _ -> ETuple l)
        <$> sep_by1 (pstoken ",") term
      in
      let term = perbinop term [ ":=" ] in
      choice [ pelet pexpr; pematch pexpr; pefun pexpr; petry pexpr; term ])
;;

let pdecl = ptoken (pdecl_base pexpr)
let pdecl_delim = many (pstoken ";;") *> pspace
let pprogram = pdecl_delim *> many (pdecl <* pdecl_delim)
let parse_program s = parse_string ~consume:Consume.All pprogram s
