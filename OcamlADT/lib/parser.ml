open Angstrom
open Ast
open Base

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init

let rec chainr1 e op =
  e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

let is_ws = function ' ' | '\t' -> true | _ -> false
let is_eol = function '\r' | '\n' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false

let is_keyword = function
  | "let" | "rec" | "fun" | "true" | "false" | "in" | "if" | "then" | "else"
   |"match" | "with" | "function" | "type" | "of" ->
      true
  | _ -> false

let empty = take_while (fun c -> is_ws c || is_eol c)
let empty1 = take_while1 (fun c -> is_ws c || is_eol c)
let token s = empty *> string s
let trim p = empty *> p
let kwd s = empty *> token s
let between l r p = l *> p <* r

(* braces *)
let lp = token "("
let rp = token ")"
let lsb = token "["
let rsb = token "]"
let comma = token ","
let colon = token ":"
let arrow = token "->"
let parens p = lp *> p <* rp

(********************** constructors **********************)
(* const constructors *)

let cint n = CInt n
let cbool b = CBool b
let cstring s = CString s

(* expr constructors *)

let econst c = EConst c
let evar id = EVar id
let etuple pl = ETuple pl
let econs e1 e2 = ECons (e1, e2)
let eif e1 e2 e3 = EIf (e1, e2, e3)
let elet binds e = ELet (binds, e)
let efunction cases = EFun (PVar "match", EMatch (EVar "match", cases))
let eapp = return (fun e1 e2 -> EApp (e1, e2))
let ematch e cases = EMatch (e, cases)
let eop o e1 e2 = EBinOp (o, e1, e2)
let elist = List.fold_right ~f:econs ~init:ENil

let efun args rhs =
  let helper p e = EFun (p, e) in
  List.fold_right args ~f:helper ~init:rhs

(* case constructors *)

let ccase p e = (p, e)
let acase id p = (id, p)
let pacase id p = PACase (acase id p)
let aconstr id ty = (id, ty)

(* binding constructor *)

let bbind isrec p e = (isrec, p, e)

(* pattern constructors *)

let pwild _ = PWild
let pvar id = PVar id
let pconst c = PConst c
let ptuple pl = PTuple pl
let popcons = token "::" *> return (fun p1 p2 -> PCons (p1, p2))
let pcons = return (fun p1 p2 -> PCons (p1, p2))
let plist = List.fold_right ~f:(fun p1 p2 -> PCons (p1, p2)) ~init:PNil

(* decl constructors *)

let dlet isrec p e = DLet (isrec, p, e)
let dadt id acases = DAdt (id, acases)

(* plain parsers *)

let choice_op ops =
  choice
    (List.map ~f:(fun (tok, cons) -> token tok *> (return @@ eop cons)) ops)

let add_sub = choice_op [("+", Add); ("-", Sub)]
let mult_div = choice_op [("*", Mul); ("/", Div)]
let cmp = choice_op [(">=", Geq); (">", Gre); ("<=", Leq); ("<", Less)]
let eq_uneq = choice_op [("=", Eq); ("<>", Neq)]
let conj = choice_op [("&&", And)]
let disj = choice_op [("||", Or)]
let cons = token "::" *> return econs

let identifier is_valid_first_char =
  let is_char_id = function
    | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
    | _ -> false in
  empty *> satisfy is_valid_first_char
  >>= fun fst ->
  let take_func = match fst with '_' -> many1 | _ -> many in
  take_func (satisfy is_char_id)
  >>= fun inner ->
  let id = Base.String.of_char_list (fst :: inner) in
  if is_keyword id then fail "Ivalid id" else return id

let id =
  let is_valid_first_char = function 'a' .. 'z' | '_' -> true | _ -> false in
  identifier is_valid_first_char

let constr_id =
  let is_valid_first_char = function 'A' .. 'Z' -> true | _ -> false in
  identifier is_valid_first_char

(* const parsing *)
let cunsint =
  trim (take_while1 is_digit)
  >>= fun helper -> return (Int.of_string helper) >>| cint

let cint =
  trim (option "" (token "+" <|> token "-"))
  >>= fun sign ->
  take_while1 is_digit
  >>= fun helper -> return (Int.of_string (sign ^ helper)) >>| cint

let cbool =
  let ttrue = kwd "true" *> return (cbool true) in
  let ffalse = kwd "false" *> return (cbool false) in
  ttrue <|> ffalse

let cstring =
  cstring <$> (kwd "\"" *> take_while (fun c -> c != '"') <* string "\"")

let const = trim (choice [cint; cbool; cstring])
let uns_const = trim (choice [cunsint; cbool; cstring])

(* pattern parsing *)
let pvar = id >>| pvar
let pwild = token "_" >>| pwild
let pconst = const >>| pconst

type pdispatch =
  { tuple: pdispatch -> pattern t
  ; cons: pdispatch -> pattern t
  ; pat: pdispatch -> pattern t
  ; adt: pdispatch -> pattern t }

let pack =
  let pat d = fix (fun _self -> trim (choice [d.tuple d; d.cons d; d.adt d])) in
  let tuple d =
    fix (fun _self ->
        trim
        @@ lift2 (fun hd tl -> hd :: tl) (d.cons d) (many1 (comma *> d.cons d))
        >>| ptuple ) in
  let adt d =
    fix (fun _self ->
        let plist =
          trim (between lsb rsb (sep_by (token ";") (d.pat d))) >>| plist in
        let prim =
          trim @@ choice [pconst; pvar; pwild; plist; parens @@ d.pat d] in
        trim
          ( lift2 pacase constr_id (option (PTuple []) (empty1 *> d.adt d))
          <|> prim ) ) in
  let cons d = fix (fun _self -> chainr1 (d.adt d) popcons) in
  {tuple; cons; pat; adt}

let pattern = pack.pat pack

(* expr parsing *)

type edispatch =
  { key: edispatch -> expr t
  ; tuple: edispatch -> expr t
  ; expr: edispatch -> expr t
  ; op: edispatch -> expr t }

let app_unop p = choice [token "+" *> p; p]

let pack =
  let key d =
    fix
    @@ fun _self ->
    let eif =
      trim
      @@ lift3 eif
           (kwd "if" *> d.expr d)
           (kwd "then" *> d.expr d)
           (kwd "else" *> d.expr d) in
    let elet =
      let binding =
        trim
        @@ lift3 bbind
             (kwd "let" *> option false (kwd "rec" >>| fun _ -> true))
             pattern
             (lift2 efun
                (empty *> many pattern <* token "=")
                (d.expr d <* kwd "in") ) in
      trim @@ lift2 elet binding (d.expr d) in
    let efun =
      trim @@ lift2 efun (kwd "fun" *> many pattern <* arrow) (d.expr d) in
    let ematch =
      let fst_case = lift2 ccase (token "|" *> pattern <* arrow) (d.expr d) in
      let other_cases = lift2 ccase (token "|" *> pattern <* arrow) (d.expr d) in
      let cases =
        lift2 (fun fst other -> fst :: other) fst_case (many other_cases) in
      let pmatch = lift2 ematch (kwd "match" *> d.expr d <* kwd "with") cases in
      let pfunction = kwd "function" *> cases >>| efunction in
      trim @@ pfunction <|> pmatch in
    choice [eif; elet; ematch; efun] in
  let tuple d =
    lift2 ( @ )
      (many1 (d.op d <* comma))
      (d.op d <|> d.key d >>| fun rhs -> [rhs])
    >>| etuple in
  let expr d = fix @@ fun _self -> trim @@ d.key d <|> d.tuple d <|> d.op d in
  let op d =
    fix
    @@ fun _self ->
    let lst = trim @@ between lsb rsb @@ sep_by (token ";") (d.expr d) in
    let prim =
      trim
      @@ choice
           [ lst >>| elist; uns_const >>| econst; id >>| evar; parens @@ d.expr d
           ; (constr_id >>| fun s -> EConstr (s, ETuple [])) ] in
    let helper op pl pr =
      let rec go acc =
        lift2 (fun f x -> f acc x) op (choice [pl >>= go; pr]) <|> return acc
      in
      pl >>= go in
    let app_op =
      trim
        ( lift2 (fun id prim -> EConstr (id, prim)) constr_id prim
        <|> chainl1 prim eapp ) in
    trim
    @@ List.fold_right
         [mult_div; add_sub; cons; cmp; eq_uneq; conj; disj]
         ~f:(fun x y -> helper x y @@ d.key d)
         ~init:app_op in
  {key; tuple; expr; op}

let expr = pack.expr pack

(* type parsing *)

let tyexp =
  fix
  @@ fun tyexpr ->
  let prim =
    trim
    @@ choice
         [ token "int" *> return TInt; token "string" *> return TString
         ; token "bool" *> return TBool; (id >>| fun s -> TAdt s); parens tyexpr
         ] in
  let list =
    prim
    >>= fun lst_ty ->
    many1 (empty1 *> token "list")
    >>= fun lst ->
    let rec wrap acc n =
      match n with 0 -> acc | _ -> wrap (TList acc) (n - 1) in
    return @@ wrap lst_ty (List.length lst) in
  let tup =
    sep_by1 (token "*" *> empty) (list <|> prim)
    >>| function [x] -> x | tpl -> TTuple tpl in
  trim @@ chainr1 tup (arrow *> return (fun t1 t2 -> TArrow (t1, t2)))

(* Decl parsing *)

let aconstr =
  lift2 aconstr
    (token "|" *> constr_id)
    (option (TTuple []) (empty1 *> token "of" *> tyexp))

let decl =
  let dlet =
    lift3 dlet
      (kwd "let" *> option false (empty1 *> kwd "rec" >>| fun _ -> true))
      (empty1 *> pattern)
      (lift2 efun (empty1 *> many pattern <* token "=") expr) in
  let dadt =
    lift2 dadt (kwd "type" *> empty1 *> id <* token "=") (many1 aconstr) in
  trim @@ (dlet <|> dadt)

(* program parser *)

let progr = many1 (trim @@ decl <* trim (many (trim (token ";;"))))
let parse_with p s = parse_string ~consume:Consume.All p s

let parse_or_error s =
  match parse_with progr s with
  | Ok ok -> ok
  | Error err ->
      Format.printf "Error while parsing: %s\n" err;
      exit 1

(* TESTS *)

let test_for_parser code expected =
  match parse_with progr code with
  | Ok ok -> (
    match List.equal equal_decl ok expected with
    | true -> true
    | false ->
        Format.printf "Expected: %a\nActual: %a\n" pp_program expected
          pp_program ok;
        false )
  | Error err ->
      Format.printf "Error: %s\n" err;
      false

let%test _ =
  test_for_parser {|
  let x = 2
|} [DLet (false, PVar "x", EConst (CInt 2))]

let%test _ =
  test_for_parser {|
  let sum x y = x + y
  let sum_of_two = sum 2
|}
    [ DLet
        ( false
        , PVar "sum"
        , EFun (PVar "x", EFun (PVar "y", EBinOp (Add, EVar "x", EVar "y"))) )
    ; DLet (false, PVar "sum_of_two", EApp (EVar "sum", EConst (CInt 2))) ]

let%test _ =
  test_for_parser
    {|
  let is_true = function
  | true -> true
  | false -> false
|}
    [ DLet
        ( false
        , PVar "is_true"
        , EFun
            ( PVar "match"
            , EMatch
                ( EVar "match"
                , [ (PConst (CBool true), EConst (CBool true))
                  ; (PConst (CBool false), EConst (CBool false)) ] ) ) ) ]

let%test _ =
  test_for_parser
    {|
     let rec map f = function
  | [] -> []
  | h :: tl -> f h :: map f tl
|}
    [ DLet
        ( true
        , PVar "map"
        , EFun
            ( PVar "f"
            , EFun
                ( PVar "match"
                , EMatch
                    ( EVar "match"
                    , [ (PNil, ENil)
                      ; ( PCons (PVar "h", PVar "tl")
                        , ECons
                            ( EApp (EVar "f", EVar "h")
                            , EApp (EApp (EVar "map", EVar "f"), EVar "tl") ) )
                      ] ) ) ) ) ]

let%test _ =
  test_for_parser {|
  let _ = 100 / 4 * (3 + (3 - 3)) / 9090
|}
    [ DLet
        ( false
        , PWild
        , EBinOp
            ( Div
            , EConst (CInt 100)
            , EBinOp
                ( Mul
                , EConst (CInt 4)
                , EBinOp
                    ( Div
                    , EBinOp
                        ( Add
                        , EConst (CInt 3)
                        , EBinOp (Sub, EConst (CInt 3), EConst (CInt 3)) )
                    , EConst (CInt 9090) ) ) ) ) ]

let%test _ =
  test_for_parser {|
  type a = |Age of int | Name of string  
|}
    [DAdt ("a", [("Age", TInt); ("Name", TString)])]

let%test _ =
  test_for_parser {|
  type a = |Age of int | Name of string
  let x = Age 2
|}
    [ DAdt ("a", [("Age", TInt); ("Name", TString)])
    ; DLet (false, PVar "x", EConstr ("Age", EConst (CInt 2))) ]

let%test _ =
  test_for_parser
    {|
     let rec map f = function
   | [] -> []
   | h :: tl -> f h :: map f tl

   let x = [1;2;3]
   let y = map (fun x -> x + 3) [1;2;3]
|}
    [ DLet
        ( true
        , PVar "map"
        , EFun
            ( PVar "f"
            , EFun
                ( PVar "match"
                , EMatch
                    ( EVar "match"
                    , [ (PNil, ENil)
                      ; ( PCons (PVar "h", PVar "tl")
                        , ECons
                            ( EApp (EVar "f", EVar "h")
                            , EApp (EApp (EVar "map", EVar "f"), EVar "tl") ) )
                      ] ) ) ) )
    ; DLet
        ( false
        , PVar "x"
        , ECons
            ( EConst (CInt 1)
            , ECons (EConst (CInt 2), ECons (EConst (CInt 3), ENil)) ) )
    ; DLet
        ( false
        , PVar "y"
        , EApp
            ( EApp
                ( EVar "map"
                , EFun (PVar "x", EBinOp (Add, EVar "x", EConst (CInt 3))) )
            , ECons
                ( EConst (CInt 1)
                , ECons (EConst (CInt 2), ECons (EConst (CInt 3), ENil)) ) ) )
    ]

let%test _ =
  test_for_parser
    {|
     let x = 1 :: 2 :: []
|}
    [ DLet
        (false, PVar "x", ECons (EConst (CInt 1), ECons (EConst (CInt 2), ENil)))
    ]

(* let%test _ = test_for_parser {|
     let x = a / b / c
   |} [ DLet
         (false, PVar "x", E)
       ] *)
