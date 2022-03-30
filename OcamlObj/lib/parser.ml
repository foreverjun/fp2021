open Angstrom
open Ast
open Base

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

let is_lower = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_upper = function
  | 'A' .. 'Z' -> true
  | _ -> false

let is_letter ch = is_lower ch || is_upper ch

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
  
let is_wild = function
  | '_' -> true
  | _ -> false
  
let is_keyword = function
  | "and" | "as" | "assert" | "asr" | "begin" | "class" | "constraint" | "do" | "done" | "downto" | "else" | "end" | "exception" | "external" | "false"
  | "for" | "fun" | "function" | "functor" | "if" | "in" | "include" | "inherit" | "initializer" | "land" | "lazy" | "let" | "lor" | "lsl" | "lsr" | "lxor"
  | "match" | "method" | "mod" | "module" | "mutable" | "new" | "nonrec" | "object" | "of" | "open" | "or" | "private" | "rec" | "sig" | "struct" | "then"
  | "to" | "true" | "try" | "type" | "val" | "virtual" | "when" | "while"| "with" -> true
  | _ -> false

let empty = take_while is_space
let empty1 = take_while1 is_space
let token s = empty *> string s
let token1 s = empty1 *> string s
let trim p = empty *> p <* empty
let parens p = token "(" *> p <* token ")"

let c_int n = CInt n
let c_string s = CString s
let c_bool b = CBool b

let econst const = EConst const
let econs expr1 expr2 = ECons (expr1, expr2)
let ebinopr bin_operator expr1 expr2 = EBinopr (bin_operator, expr1, expr2)
let evar name = EVar name
let etuple exprl = ETuple exprl
let eif expr1 expr2 expr3 = EIf (expr1, expr2, expr3)
let _elet binding expr = ELet (binding, expr)
let efunction cases = EFun (PVar "match", EMatch (EVar "match", cases))
let efun ptrn expr = EFun (ptrn, expr)
let efun args rhs = List.fold_right args ~f:efun ~init:rhs
let ematch expr cases = EMatch (expr, cases)
let eobj exprl = EObj exprl
let emeth pattern expr = EMeth (pattern , expr)
let eval pattern expr = EVal (pattern, expr)
let ecallmeth (name1 , name2) = ECallM (name1, name2)
let elist = List.fold_right ~f:econs ~init:ENil

let pwild _ = PWild
let pvar name = PVar name
let pconst c = PConst c
let ptuple pl = PTuple pl
let popcons = token "::" *> return (fun p1 p2 -> PCons (p1, p2))
let pcons = return (fun p1 p2 -> PCons (p1, p2))
let plist = List.fold_right ~f:(fun p1 p2 -> PCons (p1, p2)) ~init:PNil

let dlet isrec ptrn expr = DLet (isrec, ptrn, expr)


let choice_op ops =
  choice @@ List.map ~f:(fun (tok, cons) -> token tok *> (return @@ ebinopr cons)) ops
;;

let add_sub = choice_op [ "+", Add; "-", Sub ]
let mult_div = choice_op [ "*", Mul; "/", Div ]
let cmp = choice_op [ ">=", Greq; ">", Gre; "<=", Leeq; "<", Less ]
let eq_uneq = choice_op [ "=", Eq; "<>", Neq ]
let conj = choice_op [ "&&", And ]
let disj = choice_op [ "||", Or ]
let cons = token "::" *> return econs

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= go
;;

let procl op pl pr =
  let rec go acc =
    lift2 (fun f x -> f acc x) op (choice [ pl >>= go; pr ]) <|> return acc
  in
  pl >>= go
;;

let eapp = return (fun e1 e2 -> EApp (e1, e2))

let procr op pl pr =
  let p =
    fix @@ fun p -> pl >>= fun l -> op >>= (fun op -> p <|> pr >>| op l) <|> return l
  in
  p
;;

let eunop o e = EUnOp (o, e)

let app_unop p =
  choice
    [ token "-" *> p >>| eunop Minus; token "not" *> p >>| eunop Not; token "+" *> p; p ]
;;



let check_name check_first =
  empty *> satisfy check_first
  >>= fun fst ->
  let take_func = match fst with '_' -> take_while1 | _ -> take_while in
  take_func (fun c -> is_letter c || is_digit c || is_wild c)
  >>= fun name ->
  let concat = (Char.to_string fst)^name
  in
  match concat with 
    |"" -> fail "Wrong method call"
    |a -> if is_keyword a then fail "Wrong method call" else return a

let name = check_name (fun ch -> is_lower ch || is_wild ch)



let call_meth = 
  empty *> satisfy (fun ch -> is_lower ch || is_wild ch)
  >>= fun fst ->
  let take_func = match fst with '_' -> take_while1 | _ -> take_while in
  lift2
    (fun a b -> ((Char.to_string fst)^a,b))
    (take_func (fun ch -> is_digit ch || is_letter ch || is_wild ch) <* string "#")
    (take_func (fun ch -> is_digit ch || is_letter ch || is_wild ch))
  >>= fun call -> 
  match call with 
  | (_, "")| ("",_) -> fail "Wrong method call"
  | a,b -> if is_keyword a || is_keyword b then fail "Wrong method call" else return call

let uns = trim @@ take_while1 is_digit

let cunsint =
  let* uns = uns in
  return (Base.Int.of_string uns) >>| c_int
;;

let cint =
  let* sign = trim (option "" (token "+" <|> token "-")) in
  take_while1 is_digit
  >>= fun dight -> return (Base.Int.of_string (sign ^ dight)) >>| c_int
;;

let cbool =
  let _true = token "true" *> return (c_bool true) in
  let _false = token "false" *> return (c_bool false) in
  _true <|> _false
;;

let cstring =
  (token "\'" *> take_while (fun ch -> ch != '"') <* string "\"") >>| c_string
;;

let const = trim @@ choice [ cint; cbool; cstring ]
let uns_const = trim @@ choice [ cunsint; cbool; cstring ]

let pvar = name >>| pvar
let pwild = token "_" >>| pwild
let pconst = const >>| pconst

type pdispatch =
  { tuple: pdispatch -> pattern t
  ; other: pdispatch -> pattern t
  ; pat: pdispatch -> pattern t}

let pack =
  let pat d = fix @@ fun _self -> trim (choice [ d.tuple d; d.other d ]) in
  let tuple d =
    fix @@ fun _self ->
        trim
        @@ lift2 (fun hd tl -> hd :: tl) (d.other d) (many1 (token "," *> d.other d))
        >>| ptuple  in
  let other d =
    fix @@ fun _self ->
        let plist = trim (token "[" *> (sep_by (token ";") (d.pat d)) <* token "]") >>| plist in
        let prim = trim @@ choice [pconst; pvar; pwild; plist; parens @@ d.pat d] in
        trim @@ chainr1 prim  popcons in
  {tuple; pat; other}

let pattern = pack.pat pack

type edispatch =
  { key: edispatch -> expr t
  ; tuple: edispatch -> expr t
  ; exp: edispatch -> expr t
  ; op: edispatch -> expr t
  }

  let pack =
  let exp d = fix @@ fun _self -> trim @@ choice [d.key d ; d.tuple d ; d.op d] in
  let key d =
    fix
    @@ fun _self ->
    let eif =
      trim
      @@ lift3 eif (token "if" *> d.exp d) (token "then" *> d.exp d) (token "else" *> d.exp d)
    in
    let elet =
      let binding =
        trim
        @@ lift3
             (fun a b c -> a,b,c)
             (token "let" *> option false (token "rec" >>| fun _ -> true))
             pattern
             (lift2 efun (empty *> many pattern <* token "=") (d.exp d <* token "in"))
      in
      trim @@ lift2 _elet (binding) (d.exp d)
    in
    let eobj =
      let emethod = 
        let binding  = 
          trim
          @@ lift2
            emeth
            (token "method" *> pattern)
            (lift2 efun (empty *> many pattern <* token "=") (d.exp d))
        in
        binding
      in
      let eval = 
        trim
        @@ lift2
          eval
          (token "val" *> pvar)
          (token "=" *> d.exp d)
      in
      token "object" *> many (choice [eval; emethod]) <* token "end" >>| eobj
    in
    let efun = trim @@ lift2 efun (token "fun" *> many pattern <* token "->") (d.exp d) in
    let ematch =
      let fst_case = lift2 (fun a b -> a,b) (option "" (token "|") *> pattern <* token "->") (d.exp d) in
      let other_cases = lift2 (fun a b -> a,b) (token "|" *> pattern <* token "->") (d.exp d) in
      let cases = lift2 (fun fst other -> fst :: other) fst_case (many other_cases) in
      let pmatch = lift2 ematch (token "match" *> d.exp d <* token "with") cases in
      let pfunction = token "function" *> cases >>| efunction in
      trim @@ pfunction <|> pmatch
    in
    choice [ elet; eif; ematch; efun; eobj]
  in
  let tuple d =
    lift2 ( @ ) (many1 (d.op d <* token ",")) (d.op d <|> d.key d >>| fun rhs -> [ rhs ])
    >>| etuple
  in
  let op d =
    fix
    @@ fun _self ->
    let lst = trim (token "[" *> (sep_by (token ";") (d.exp d)) <* token "]")  in
    let prim =
      trim
      @@ choice [ lst >>| elist; uns_const >>| econst; call_meth >>| ecallmeth; name >>| evar; parens @@ d.exp d ]
    in
    let app_op =
      trim @@ chainl1 prim eapp 
    in
    let mul_op = procl mult_div app_op @@ d.key d in
    let add_op = procl add_sub (app_unop mul_op) (app_unop @@ d.key d) in
    let cons_op = procr cons add_op @@ d.key d in
    let cmp_op = procl cmp cons_op @@ d.key d in
    let eq_op = procl eq_uneq cmp_op @@ d.key d in
    let conj_op = procl conj eq_op @@ d.key d in
    trim @@ procl disj conj_op @@ d.key d
  in
  { key; tuple; exp; op }
;;

let expr = pack.exp pack




let decl =
  let dlet =
    lift3
      dlet
      (token "let" *> option false (token "rec" >>| fun _ -> true))
      pattern
      (lift2 efun (empty *> many pattern <* token "=") expr)
  in
  dlet
;;

(*-------------- Prog parsing --------------*)

let pprog (l : decl list) : progr = l
let prog = sep_by1 (token ";;") decl <* option "" @@ trim (token ";;") >>| pprog
let parse p s = parse_string ~consume:All p s
