open Ast
open Opal

let apply_parser parser input = parse parser (LazyStream.of_string input)

let reserved_keywords =
  [ "class"
  ; "else"
  ; "if"
  ; "false"
  ; "true"
  ; "fun"
  ; "null"
  ; "return"
  ; "super"
  ; "this"
  ; "val"
  ; "var"
  ; "while"
  ; "init"
  ; "abstract"
  ; "companion"
  ; "open"
  ; "override"
  ; "private"
  ; "protected"
  ; "public"
  ]
;;

let parens parser = between (token "(") (token ")") parser
let braces parser = between (token "{") (token "}") parser

let modifiers =
  many
    (choice
       [ token "public" >> return Public
       ; token "private" >> return Private
       ; token "protected" >> return Protected
       ; token "open" >> return Open
       ; token "override" >> return Override
       ])
;;

let variable_type_modifier =
  lexeme (choice [ token "val" >> return Val; token "var" >> return Var ])
;;

let parse_identifier =
  lexeme (letter <~> many alpha_num)
  => implode
  >>= function
  | x when List.mem x reserved_keywords -> mzero
  | x -> return x
;;

let parse_typename =
  parse_identifier
  >>= function
  | "Int" -> return Int
  | "String" -> return String
  | "Boolean" -> return Boolean
  | str_typename ->
    return (Class str_typename)
    >>= fun parsed_typename ->
    exactly '?' >> return (Nullable parsed_typename) <|> return parsed_typename
;;

module Expression = struct
  open Ast

  let var_identifier =
    parse_identifier
    >>= (function
          | x when List.mem x reserved_keywords -> mzero
          | x -> return x)
    >>= fun x -> return (VarIdentifier x)
  ;;

  let int_value = many1 digit => implode % int_of_string >>= fun x -> return (IntValue x)

  let string_value =
    between (exactly '"') (exactly '"') (many any)
    => implode
    >>= fun x -> return (StringValue x)
  ;;

  let boolean_value =
    token "true"
    <|> token "false"
    >>= fun x -> return (if x == "true" then BooleanValue true else BooleanValue false)
  ;;

  let null_value = token "null" >> return NullValue

  let parsed_value =
    spaces
    >> int_value
    <|> boolean_value
    <|> string_value
    <|> null_value
    => fun x -> Const x
  ;;

  (* binary operations *)
  let add_op = token "+" >> return (fun x y -> Add (x, y))
  let sub_op = token "-" >> return (fun x y -> Sub (x, y))
  let mul_op = token "*" >> return (fun x y -> Mul (x, y))
  let div_op = token "/" >> return (fun x y -> Div (x, y))
  let mod_op = token "%" >> return (fun x y -> Mod (x, y))
  let and_op = token "&&" >> return (fun x y -> And (x, y))
  let or_op = token "||" >> return (fun x y -> Or (x, y))
  let equal_op = token "==" >> return (fun x y -> Equal (x, y))
  let not_equal_op = token "!=" >> return (fun x y -> Not (Equal (x, y)))
  let less_op = token "<" >> return (fun x y -> Less (x, y))
  let great_op = token ">" >> return (fun x y -> Less (y, x))
  let less_or_equal_op = token "<=" >> return (fun x y -> Or (Equal (x, y), Less (x, y)))
  let great_or_equal_op = token ">=" >> return (fun x y -> Or (Equal (x, y), Less (y, x)))

  (* expression parser *)
  let rec expression input = compare_expression input

  and compare_expression input =
    (chainl1
       or_expression
       (choice
          [ equal_op
          ; not_equal_op
          ; less_op
          ; great_op
          ; less_or_equal_op
          ; great_or_equal_op
          ]))
      input

  and or_expression input = (chainl1 and_expression or_op) input
  and and_expression input = (chainl1 add_expression and_op) input
  and add_expression input = (chainl1 mul_expression (choice [ add_op; sub_op ])) input

  and mul_expression input =
    (chainl1 (unar_expression <|> highest_prior_expression) mul_op) input

  and highest_prior_expression input =
    (parens expression <|> parsed_value <|> var_identifier) input

  and unar_expression input =
    let parse_not_op =
      token "!" >> lexeme highest_prior_expression >>= fun x -> return (Not x)
    in
    let parse_function_call =
      parse_identifier
      >>= fun identifier ->
      parens (sep_by expression (token ","))
      >>= fun args -> return (FunctionCall (identifier, args))
    in
    (choice [ parse_not_op; parse_function_call ]) input
  ;;
end

module Statement = struct
  open Expression
  open Ast

  let rec expression_statement input =
    (expression >>= fun expr -> return (Expression expr)) input

  and statement input =
    (choice
       [ block_statement
       ; var_declaration_statement
       ; fun_declaration_statement
       ; while_statement
       ; if_statement
       ; assign_statement
       ; return_statement
       ; expression_statement
       ])
      input

  and block_statement input =
    (braces (sep_by statement (skip_many (exactly ' ') >> newline))
    >>= fun expressions -> return (Block expressions))
      input

  and var_declaration_statement input =
    (modifiers
    >>= fun modifier_list ->
    variable_type_modifier
    >>= fun type_modifier ->
    parse_identifier
    >>= fun identifier ->
    token ":"
    >> parse_typename
    >>= fun parsed_typename ->
    option
      (VarDeclaration (modifier_list, type_modifier, identifier, parsed_typename, None))
      (token "="
      >> expression
      >>= fun parsed_expression ->
      return
        (VarDeclaration
           ( modifier_list
           , type_modifier
           , identifier
           , parsed_typename
           , Some parsed_expression ))))
      input

  and fun_declaration_statement input =
    (modifiers
    >>= fun modifier_list ->
    token "fun"
    >> parse_identifier
    >>= fun fun_identifier ->
    parens
      (sep_by
         (parse_identifier
         >>= fun var_identifier ->
         token ":"
         >> parse_typename
         >>= fun var_typename -> return (var_identifier, var_typename))
         (token ","))
    >>= fun args ->
    token ":"
    >> parse_typename
    >>= fun fun_typename ->
    block_statement
    >>= fun fun_statement ->
    return
      (FunDeclaration (modifier_list, fun_identifier, args, fun_typename, fun_statement))
    )
      input

  and assign_statement input =
    (parse_identifier
    >>= fun var_identifier ->
    token "="
    >> expression
    >>= fun assign_expression -> return (Assign (var_identifier, assign_expression)))
      input

  and if_statement input =
    (token "if"
    >> parens expression
    >>= fun if_expresssion ->
    block_statement
    >>= fun if_statement ->
    option
      (If (if_expresssion, if_statement, None))
      (token "else"
      >> block_statement
      >>= fun else_statement ->
      return (If (if_expresssion, if_statement, Some else_statement))))
      input

  and while_statement input =
    (token "while"
    >> parens expression
    >>= fun while_expression ->
    block_statement
    >>= fun while_statement -> return (While (while_expression, while_statement)))
      input

  and return_statement input =
    (token "return" >> expression >>= fun expr -> return (Return expr)) input
  ;;
end