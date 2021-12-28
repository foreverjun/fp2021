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

let parse_modifiers =
  many
    (choice
       [ token "public" >> return Public
       ; token "private" >> return Private
       ; token "protected" >> return Protected
       ; token "open" >> return Open
       ; token "override" >> return Override
       ])
;;

let parse_variable_type_modifier =
  lexeme (choice [ token "val" >> return Val; token "var" >> return Var ])
;;

let parse_identifier =
  lexeme (letter <|> exactly '_' <~> many (alpha_num <|> exactly '_'))
  => implode
  >>= function
  | x when List.mem x reserved_keywords -> mzero
  | x -> return x
;;

let rec parse_typename input =
  (parens (sep_by parse_typename (token ","))
  >>= (fun arg_typenames ->
        token "->"
        >> parse_typename
        >>= fun fun_typename -> return (FunctionType (arg_typenames, fun_typename)))
  <|> (parse_identifier
      >>= fun identifier ->
      (match identifier with
      | "Int" -> return Int
      | "String" -> return String
      | "Boolean" -> return Boolean
      | str_typename -> return (ClassIdentifier str_typename))
      >>= fun parsed_typename ->
      exactly '?' >> return (Nullable parsed_typename) <|> return parsed_typename))
    input
;;

module rec Expression : sig
  val parse_var_identifier : char Opal.input -> (Ast.expression * char Opal.input) option
  val parse_int_value : char Opal.input -> (Ast.value * char Opal.input) option
  val parse_string_value : char Opal.input -> (Ast.value * char Opal.input) option
  val parse_boolean_value : char Opal.input -> (Ast.value * char Opal.input) option
  val parse_null_value : char Opal.input -> (Ast.value * char Opal.input) option
  val parse_const_value : char Opal.input -> (Ast.expression * char Opal.input) option

  val add_op
    :  char Opal.input
    -> ((Ast.expression -> Ast.expression -> Ast.expression) * char Opal.input) option

  val sub_op
    :  char Opal.input
    -> ((Ast.expression -> Ast.expression -> Ast.expression) * char Opal.input) option

  val mul_op
    :  char Opal.input
    -> ((Ast.expression -> Ast.expression -> Ast.expression) * char Opal.input) option

  val div_op
    :  char Opal.input
    -> ((Ast.expression -> Ast.expression -> Ast.expression) * char Opal.input) option

  val mod_op
    :  char Opal.input
    -> ((Ast.expression -> Ast.expression -> Ast.expression) * char Opal.input) option

  val and_op
    :  char Opal.input
    -> ((Ast.expression -> Ast.expression -> Ast.expression) * char Opal.input) option

  val or_op
    :  char Opal.input
    -> ((Ast.expression -> Ast.expression -> Ast.expression) * char Opal.input) option

  val equal_op
    :  char Opal.input
    -> ((Ast.expression -> Ast.expression -> Ast.expression) * char Opal.input) option

  val not_equal_op
    :  char Opal.input
    -> ((Ast.expression -> Ast.expression -> Ast.expression) * char Opal.input) option

  val less_op
    :  char Opal.input
    -> ((Ast.expression -> Ast.expression -> Ast.expression) * char Opal.input) option

  val great_op
    :  char Opal.input
    -> ((Ast.expression -> Ast.expression -> Ast.expression) * char Opal.input) option

  val less_or_equal_op
    :  char Opal.input
    -> ((Ast.expression -> Ast.expression -> Ast.expression) * char Opal.input) option

  val great_or_equal_op
    :  char Opal.input
    -> ((Ast.expression -> Ast.expression -> Ast.expression) * char Opal.input) option

  val dereference_op
    :  char Opal.input
    -> ((Ast.expression -> Ast.expression -> Ast.expression) * char Opal.input) option

  val expression : char Opal.input -> (Ast.expression * char Opal.input) option
  val compare_expression : char Opal.input -> (Ast.expression * char Opal.input) option
  val or_expression : char Opal.input -> (Ast.expression * char Opal.input) option
  val and_expression : char Opal.input -> (Ast.expression * char Opal.input) option
  val add_expression : char Opal.input -> (Ast.expression * char Opal.input) option
  val mul_expression : char Opal.input -> (Ast.expression * char Opal.input) option

  val highest_prior_expression
    :  char Opal.input
    -> (Ast.expression * char Opal.input) option

  val not_expression : char Opal.input -> (Ast.expression * char Opal.input) option

  val function_call_expression
    :  char Opal.input
    -> (Ast.expression * char Opal.input) option

  val dereference_expression
    :  char Opal.input
    -> (Ast.expression * char Opal.input) option

  val unar_expression : char Opal.input -> (Ast.expression * char Opal.input) option
end = struct
  open Ast

  let parse_var_identifier =
    parse_identifier
    >>= (function
          | x when List.mem x reserved_keywords -> mzero
          | x -> return x)
    >>= fun x -> return (VarIdentifier x)
  ;;

  let parse_int_value =
    option '+' (exactly '-')
    <~> many1 digit
    => implode % int_of_string
    >>= fun x -> return (IntValue x)
  ;;

  let parse_string_value =
    skip_many space
    >> between
         (exactly '"')
         (exactly '"')
         (many (satisfy (fun x -> not (Char.equal '"' x))))
    => implode
    >>= fun x -> return (StringValue x)
  ;;

  let parse_boolean_value =
    token "true"
    <|> token "false"
    >>= fun x -> return (if x == "true" then BooleanValue true else BooleanValue false)
  ;;

  let parse_null_value = token "null" >> return NullValue

  let parse_const_value =
    spaces
    >> parse_int_value
    <|> parse_boolean_value
    <|> parse_string_value
    <|> parse_null_value
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
  let dereference_op = token "." >> return (fun x y -> Dereference (x, y))
  let elvis_dereference_op = token "?." >> return (fun x y -> ElvisDereference (x, y))

  (* expression parser *)
  let rec expression input = or_expression input

  and compare_expression input =
    (chainl1
       add_expression
       (choice
          [ equal_op
          ; not_equal_op
          ; less_or_equal_op
          ; great_or_equal_op
          ; less_op
          ; great_op
          ]))
      input

  and or_expression input = (chainl1 and_expression or_op) input
  and and_expression input = (chainl1 compare_expression and_op) input
  and add_expression input = (chainl1 mul_expression (choice [ add_op; sub_op ])) input

  and mul_expression input =
    (chainl1
       (unar_expression <|> highest_prior_expression)
       (choice [ mul_op; div_op; mod_op ]))
      input

  and highest_prior_expression input =
    (choice
       [ parens expression
       ; anonymous_function_expression
       ; parse_const_value
       ; dereference_expression
       ; function_call_expression
       ; parse_var_identifier
       ])
      input

  and not_expression input =
    (token "!" >> lexeme highest_prior_expression >>= fun x -> return (Not x)) input

  and function_call_expression input =
    (parse_identifier
    >>= fun identifier ->
    parens (sep_by expression (token ","))
    >>= fun args ->
    if String.equal identifier "println"
    then if List.length args = 1 then return (Println (List.hd args)) else mzero
    else return (FunctionCall (identifier, args)))
      input

  and dereference_expression input =
    (chainr1
       (function_call_expression <|> parse_var_identifier)
       (elvis_dereference_op <|> dereference_op))
      input

  and unar_expression input = (choice [ not_expression; function_call_expression ]) input

  and anonymous_function_expression input =
    (Statement.anonymous_function_statement
    >>= fun stat -> return (AnonymousFunctionDeclaration stat))
      input
  ;;
end

and Statement : sig
  val expression_statement : char Opal.input -> (Ast.statement * char Opal.input) option
  val statement : char Opal.input -> (Ast.statement * char Opal.input) option

  val initialize_block_statement
    :  char Opal.input
    -> (Ast.statement * char Opal.input) option

  val block_statement : char Opal.input -> (Ast.statement * char Opal.input) option

  val class_declaration_statement
    :  char Opal.input
    -> (Ast.statement * char Opal.input) option

  val var_declaration_statement
    :  char Opal.input
    -> (Ast.statement * char Opal.input) option

  val fun_declaration_statement
    :  char Opal.input
    -> (Ast.statement * char Opal.input) option

  val assign_statement : char Opal.input -> (Ast.statement * char Opal.input) option
  val if_statement : char Opal.input -> (Ast.statement * char Opal.input) option
  val while_statement : char Opal.input -> (Ast.statement * char Opal.input) option
  val return_statement : char Opal.input -> (Ast.statement * char Opal.input) option

  val anonymous_function_statement
    :  char Opal.input
    -> (Ast.statement * char Opal.input) option
end = struct
  open Expression
  open Ast

  let rec expression_statement input =
    (expression >>= fun expr -> return (Expression expr)) input

  and statement input =
    (choice
       [ block_statement
       ; anonymous_function_statement
       ; fun_declaration_statement
       ; class_declaration_statement
       ; var_declaration_statement
       ; while_statement
       ; if_statement
       ; assign_statement
       ; return_statement
       ; expression_statement
       ])
      input

  and initialize_block_statement input =
    (skip_many (exactly ' ')
    >> sep_by statement (skip_many (exactly ' ') >> newline)
    >>= fun expressions -> return (InitializeBlock expressions))
      input

  and block_statement input =
    (skip_many (exactly ' ')
    >> braces (sep_by statement (skip_many (exactly ' ') >> newline))
    >>= fun expressions -> return (Block expressions))
      input

  and class_declaration_statement input =
    (parse_modifiers
    >>= fun modifier_list ->
    token "class"
    >> parse_identifier
    >>= fun identifier ->
    parens
      (sep_by
         (parse_identifier
         >>= fun parse_var_identifier ->
         token ":"
         >> parse_typename
         >>= fun var_typename -> return (parse_var_identifier, var_typename))
         (token ","))
    >>= fun constructor_args ->
    option
      None
      (token ":" >> function_call_expression >>= fun fun_call -> return (Some fun_call))
    >>= fun super_class_constructor ->
    block_statement
    >>= fun class_statement ->
    return
      (ClassDeclaration
         ( modifier_list
         , identifier
         , constructor_args
         , super_class_constructor
         , class_statement )))
      input

  and var_declaration_statement input =
    (parse_modifiers
    >>= fun modifier_list ->
    parse_variable_type_modifier
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
    (parse_modifiers
    >>= fun modifier_list ->
    token "fun"
    >> parse_identifier
    >>= fun fun_identifier ->
    parens
      (sep_by
         (parse_identifier
         >>= fun parse_var_identifier ->
         token ":"
         >> parse_typename
         >>= fun var_typename -> return (parse_var_identifier, var_typename))
         (token ","))
    >>= fun args ->
    option Unit (token ":" >> parse_typename)
    >>= fun fun_typename ->
    block_statement
    >>= fun fun_statement ->
    return
      (FunDeclaration (modifier_list, fun_identifier, args, fun_typename, fun_statement))
    )
      input

  and assign_statement input =
    (expression
    >>= fun identifier_expression ->
    token "="
    >> expression
    >>= fun assign_expression ->
    return (Assign (identifier_expression, assign_expression)))
      input

  and if_statement input =
    (token "if"
    >> parens expression
    >>= fun if_expresssion ->
    block_statement
    <|> expression_statement
    >>= fun if_statement ->
    option
      (If (if_expresssion, if_statement, None))
      (token "else"
      >> (block_statement <|> expression_statement)
      >>= fun else_statement ->
      return (If (if_expresssion, if_statement, Some else_statement))))
      input

  and while_statement input =
    (token "while"
    >> parens expression
    >>= fun while_expression ->
    block_statement
    <|> expression_statement
    >>= fun while_statement -> return (While (while_expression, while_statement)))
      input

  and return_statement input =
    (token "return" >> expression >>= fun expr -> return (Return expr)) input

  and anonymous_function_statement input =
    (braces
       (option
          []
          (sep_by
             (parse_identifier
             >>= fun parse_var_identifier ->
             token ":"
             >> parse_typename
             >>= fun var_typename -> return (parse_var_identifier, var_typename))
             (token ","))
       >>= fun args ->
       (if List.length args > 0 then token "->" else token "")
       >> sep_by statement (skip_many (exactly ' ') >> newline)
       >>= fun statements -> return (args, statements))
    >>= fun (args, statements) ->
    return (AnonymousFunctionDeclarationStatement (args, Block statements)))
      input
  ;;
end