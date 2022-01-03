open Angstrom
open Ast

(** Mini Assembler Parser. *)

(** [spaces] accepts input as long as it's space and discards
    the accepted characters. *)
let spaces =
  let is_space = function
    | '\t' | '\x11' | '\x12' | '\r' | '\x26' | ' ' -> true
    | _ -> false in
  skip_while is_space

(** [is_blank c] accepts any character and returns whether [c] is blank. *)
let is_blank = function
  | '\t' | '\x11' | '\x12' | '\r' | '\x26' | ' ' | '\n' -> true
  | _ -> false

(** [eol] accepts input as long as it's blank or comment and discards
    the accepted characters. *)
let eol =
  let blanks = skip_while is_blank in
  let is_eol c = c = '\n' in
  blanks
  *> many
       ( char ';' *> skip_while (fun c -> not (is_eol c))
       <|> skip is_eol *> blanks )

(** [trim x] accepts input and discards space characters around. *)
let trim x = spaces *> x <* spaces

(** [is_digit] accepts any character and returns whether it's digit. *)
let is_digit = function '0' .. '9' -> true | _ -> false

(** [is_register] accepts string and returns whether it's register. *)
let is_register = function
  (* 8 bit *)
  | "AH" | "AL" | "BH" | "BL" | "CH" | "CL" | "DH" | "DL"
  (* 16 bit *)
   |"AX" | "BX" | "CX" | "DX"
  (* 32 bit *)
  (* General-purpose *)
   |"EAX" | "EBX" | "ECX" | "EDX" | "ESI" | "EDI"
  (* Stack pointer *)
   |"ESP"
  (* Base pointer *)
   |"EBP"
  (* 64 bit *)
   |"RAX" | "RBX" | "RCX" | "RDX" | "RSP" | "RBP" | "RSI" | "RDI"
  (* 128 bit SSE *)
   |"XMM0" | "XMM1" | "XMM2" | "XMM3" | "XMM4" | "XMM5" | "XMM6" | "XMM7" ->
      true
  | _ -> false

(** [is_mnemonic] accepts string and returns whether it's assembler instruction. *)
let is_mnemonic = function
  (* --- Movement --- *)
  | "MOV"
  (*
      mov <reg>,<reg>
      mov <reg>,<mem>
      mov <reg>,<con>
  *)
   |"PUSH"
  (*
      push <label> 
  *)
   |"POP"
  (*
      pop <reg32>
  *)
  (* --- Arithmetic and Logic --- *)
   |"ADD" | "SUB"
  (*
      add/sub <reg>,<reg>
      add/sub <reg>,<mem>
      add/sub <reg>,<con>
  *)
   |"INC" | "DEC"
  (*
      inc/dec <reg>
  *)
   |"IMUL"
  (*
      imul <reg32>,<reg32>
      imul <reg32>,<mem>
  *)
   |"AND" | "OR" | "XOR"
  (*
      and/or/xor <reg>,<reg>
      and/or/xor <reg>,<mem>
      and/or/xor <reg>,<con>
  *)
   |"NOT" | "NEG"
  (*
      not/neg <reg>
  *)
   |"SHL" | "SHR"
  (*
      shl/shr <reg>,<con8>
  *)
  (* --- Control Flow --- *)
   |"JMP"
  (*
      jmp <label>
  *)
   |"JE" | "JNE" | "JG" | "JGE" | "JL" | "JLE"
  (*
      j* <label>
  *)
   |"CMP"
  (*
      cmp <reg>,<reg>
      cmp <reg>,<mem>
      cmp <reg>,<con>
  *)
   |"CALL"
  (*
      call <label>
  *)
   |"RET"
  (*
      ret
  *)
   |"SYSCALL"
  (*
      syscall
  *)
  (* --- SSE --- *)
   |"ADDPD" | "SUBPD" | "MULPD"
   |"MOVAPD"
    (*
       movapd <regSSE>,<regSSE>
       movapd <regSSE>,<mem>
       movapd <regSSE>,<con>
  *)
    ->
      true
  | _ -> false

(** [is_section] accepts string and returns whether it's assembler section name. *)
let is_section = function
  | "CODE" | "TEXT" | "DATA" | "CONST" -> true
  | _ -> false

(** [is_data_decl] accepts string and returns whether it's assembler data type. *)
let is_data_decl = function "DB" | "DW" | "DD" | "DQ" -> true | _ -> false

(** [take_id] accepts input returns [string] if it is assembler label otherwise fails. *)
let take_id =
  let check_fst = function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' | '@' | '$' | '?' -> true
    | _ -> false in
  let check_inner c = check_fst c || is_digit c in
  satisfy check_fst
  >>= fun fst ->
  take_while check_inner <* string ":" <|> take_while check_inner
  >>= fun inner ->
  let s = Char.escaped fst ^ inner in
  if is_register s || is_mnemonic s || is_data_decl s then fail "Invalid label"
  else return s

(** [expr_p] parses [Ast.Add], [Ast.Sub], [Ast.Mul], [Ast.Div], [Ast.Mod], [Ast.Shl], [Ast.Shr], [Ast.Reg], [Ast.Label] and [Ast.Const]. *)
let expr_p =
  fix (fun expr ->
      let parens p = trim (char '(' *> trim p <* char ')') in
      let digit =
        let hexNumber =
          option "" (string "+" <|> string "-")
          >>= fun sign ->
          spaces *> string "0x"
          *> option "0"
               (take_while1 (function
                 | '0' .. '9' -> true
                 | 'A' .. 'F' -> true
                 | 'a' .. 'f' -> true
                 | _ -> false ) )
          >>| fun s -> Int64.of_string (String.concat "" [sign; "0x"; s]) in
        let decNumber =
          option "" (string "+" <|> string "-")
          >>= fun sign ->
          spaces *> take_while1 is_digit >>| fun s -> Int64.of_string (sign ^ s)
        in
        hexNumber <|> decNumber >>| fun x -> Const x in
      let label =
        take_id
        >>| fun s ->
        if is_register (String.uppercase_ascii s) then
          Reg (String.uppercase_ascii s)
        else Label s in
      let factor = parens expr <|> trim digit <|> trim label in
      let add = char '+' *> return (fun x y -> Add (x, y)) in
      let sub = char '-' *> return (fun x y -> Sub (x, y)) in
      let mul = char '*' *> return (fun x y -> Mul (x, y)) in
      let div = char '/' *> return (fun x y -> Div (x, y)) in
      let mmod = char '%' *> return (fun x y -> Mod (x, y)) in
      let shl = string "<<" *> return (fun x y -> Shl (x, y)) in
      let shr = string ">>" *> return (fun x y -> Shr (x, y)) in
      let chainl1 e op =
        let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
        e >>= fun init -> go init in
      let term = trim (chainl1 factor (mul <|> div <|> shr <|> shl <|> mmod)) in
      trim @@ chainl1 term (add <|> sub) )

(** [init_value_p] parses [Ast.Expr], [Ast.Str] and [Ast.Dup]. *)
let init_value_p =
  let mstring =
    let quote s =
      trim (char '\"' *> s <* char '\"' <|> (char '\'' *> s <* char '\''))
      >>| fun s -> s in
    quote (take_till (function '"' | '\'' -> true | _ -> false)) in
  let dup =
    expr_p
    >>= fun e ->
    spaces *> string_ci "DUP" *> spaces *> mstring >>| fun s -> Dup (e, s) in
  dup <|> (expr_p >>| fun x -> Expr x) <|> (mstring >>| fun s -> Str s)

(** [mnemonic_p] parses [Ast.Mnemonic]. *)
let mnemonic_p =
  take_till (fun c -> is_blank c || c = ';')
  >>= function
  | s when is_mnemonic (String.uppercase_ascii s) ->
      return (Mnemonic (String.uppercase_ascii s))
  | _ -> fail "Invalid command"

(** [in_segment_dir_p] parses [Ast.Instruction] and [Ast.DataDecl]. *)
let in_segment_dir_p =
  let separators = spaces *> char ',' *> spaces in
  let data_dir =
    take_till is_blank
    >>= function
    | s when is_data_decl (String.uppercase_ascii s) ->
        spaces *> sep_by separators init_value_p
        <* spaces
        >>= fun values -> return @@ DataDecl (String.uppercase_ascii s, values)
    | _ -> fail "Invalid declaration" in
  let instruction =
    mnemonic_p
    >>= fun cmd ->
    spaces *> sep_by separators expr_p
    <* spaces
    >>| fun exprs -> Instruction (cmd, exprs) in
  instruction <|> data_dir

(** [in_sec_dir_p] parses [Ast.InDir], [Ast.EquDir] and [Ast.EqualDir]. *)
let in_sec_dir_p =
  let id_p = take_id >>| fun s -> Id s in
  let in_sec_dir = in_segment_dir_p >>| fun isd -> InDir (None, isd) in
  let in_sec_dir_label =
    spaces *> id_p <* eol
    >>= fun id -> spaces *> in_segment_dir_p >>| fun isd -> InDir (Some id, isd)
  in
  let general_dir =
    spaces *> id_p
    >>= fun id ->
    spaces
    *> ( string "equ" *> spaces *> expr_p
       <* spaces
       >>| fun exp -> EquDir (id, exp) )
    <|> spaces
        *> ( string "=" *> spaces *> expr_p
           <* spaces
           >>| fun exp -> EqualDir (id, exp) ) in
  in_sec_dir <|> in_sec_dir_label <|> general_dir

(** [sec_dir_p] parses [Ast.Section]. *)
let sec_dir_p =
  eol
  *> option "none"
       ( string_ci "SECTION" *> spaces *> char '.'
         *> take_till (fun c -> is_blank c || c = ';')
       >>| fun s -> String.uppercase_ascii s )
  >>= fun s ->
  let f sep sec =
    eol *> sep eol in_sec_dir_p
    >>= fun values -> eol *> return (Section (sec, values)) in
  match s with
  | str when is_section str -> f sep_by s
  | n when n = "none" -> f sep_by1 "TEXT"
  | _ -> fail "Invalid section"

(** [directive_p] parses [Ast.Directive]. *)
let directive_p = many sec_dir_p >>| fun s -> Directive s

(** [parse] runs [directive_p] on [s]. The parser will receive an
    [`Eof] after all of [s] has been consumed. *)
let parse = parse_string ~consume:Prefix directive_p

(** / **)

(********************************* TESTS ****************************************)

let parse_test p s = parse_string ~consume:All p s

let parse_test_suc pp p s exp =
  match parse_test p s with
  | Error _ -> false
  | Ok res when exp = res -> true
  | Ok res ->
      let open Format in
      print_string "\nActual is:\n";
      pp std_formatter res;
      false

let parse_test_fail pp p s =
  match parse_test p s with
  | Error _ -> true
  | Result.Ok res ->
      let open Format in
      print_string "\nActual is:\n";
      pp std_formatter res;
      false

let expr_test_suc = parse_test_suc pp_expr expr_p
let expr_test_fail = parse_test_fail pp_expr expr_p

let%test _ = expr_test_suc "10" (Const 10L)
let%test _ = expr_test_suc "10 + 20" (Add (Const 10L, Const 20L))

let%test _ =
  expr_test_suc "10 + 10 * 19" (Add (Const 10L, Mul (Const 10L, Const 19L)))

let%test _ =
  expr_test_suc "10 * 10 - 19" (Sub (Mul (Const 10L, Const 10L), Const 19L))

let%test _ =
  expr_test_suc "1 * ( 0x10 << 19 )" (Mul (Const 1L, Shl (Const 16L, Const 19L)))

let%test _ =
  expr_test_suc "1 *  0x10 >> 19 " (Shr (Mul (Const 1L, Const 16L), Const 19L))

let%test _ =
  expr_test_suc "1 <<  0x10 >> 19 " (Shr (Shl (Const 1L, Const 16L), Const 19L))

let%test _ =
  expr_test_suc "1 >>   0x10 << 19 " (Shl (Shr (Const 1L, Const 16L), Const 19L))

let%test _ = expr_test_suc "(( 0x) ) " (Const 0L)
let%test _ = expr_test_suc "(( RaX ) ) " (Reg "RAX")
let%test _ = expr_test_suc "(( RaX2 ) ) " (Label "RaX2")
let%test _ = expr_test_suc "( RaX2  ) " (Label "RaX2")
let%test _ = expr_test_fail "( RaX2 + - ) "

let init_value_test_suc = parse_test_suc pp_init_value init_value_p
let init_value_test_fail = parse_test_fail pp_init_value init_value_p

let%test _ = init_value_test_suc "10" (Expr (Const 10L))

let%test _ =
  init_value_test_suc "  10 + 20  " (Expr (Add (Const 10L, Const 20L)))

let%test _ =
  init_value_test_suc "10 + 10 * 19"
    (Expr (Add (Const 10L, Mul (Const 10L, Const 19L))))

let%test _ =
  init_value_test_suc "10 * 10 - 19"
    (Expr (Sub (Mul (Const 10L, Const 10L), Const 19L)))

let%test _ =
  init_value_test_suc "1 * ( 0x10 << 19 )"
    (Expr (Mul (Const 1L, Shl (Const 16L, Const 19L))))

let%test _ = init_value_test_suc "(( 0x) ) " (Expr (Const 0L))
let%test _ = init_value_test_suc "(( RaX ) ) " (Expr (Reg "RAX"))
let%test _ = init_value_test_suc "(( RaX2 ) ) " (Expr (Label "RaX2"))
let%test _ = init_value_test_suc "( RaX2  ) " (Expr (Label "RaX2"))
let%test _ = init_value_test_suc "'0x'" (Str "0x")
let%test _ = init_value_test_suc "\"RaX\"" (Str "RaX")
let%test _ = init_value_test_fail "\'RaX\""
let%test _ = init_value_test_suc "6 DuP \'5\'" (Dup (Const 6L, "5"))

let%test _ =
  init_value_test_suc "  6 + 1 DUP \'5\'    "
    (Dup (Add (Const 6L, Const 1L), "5"))

let in_segment_dir_test_suc = parse_test_suc pp_in_segment_dir in_segment_dir_p

let in_segment_dir_test_fail =
  parse_test_fail pp_in_segment_dir in_segment_dir_p

let%test _ =
  in_segment_dir_test_suc "add rax,rax1"
    (Instruction (Mnemonic "ADD", [Reg "RAX"; Label "rax1"]))

let%test _ = parse_test_fail pp_mnemonic mnemonic_p "addq"

let%test _ =
  in_segment_dir_test_suc "db \"fgfg\"" (DataDecl ("DB", [Str "fgfg"]))

let%test _ =
  in_segment_dir_test_suc "MOv rax,      5"
    (Instruction (Mnemonic "MOV", [Reg "RAX"; Const 5L]))

let in_sec_dir_test_suc = parse_test_suc pp_in_sec_dir in_sec_dir_p
let in_sec_dir_test_fail = parse_test_fail pp_in_sec_dir in_sec_dir_p

let%test _ =
  in_sec_dir_test_suc "der  add"
    (InDir (Some (Id "der"), Instruction (Mnemonic "ADD", [])))

let%test _ =
  in_sec_dir_test_suc "add" (InDir (None, Instruction (Mnemonic "ADD", [])))

let%test _ =
  in_sec_dir_test_suc "l:   = 1+1 " (EqualDir (Id "l", Add (Const 1L, Const 1L)))

let%test _ = in_sec_dir_test_fail "addr addr"

let sec_dir_test_suc = parse_test_suc pp_sec_dir sec_dir_p
let sec_dir_test_fail = parse_test_fail pp_sec_dir sec_dir_p

let%test _ =
  sec_dir_test_suc "SecTion .text \nadd"
    (Section ("TEXT", [InDir (None, Instruction (Mnemonic "ADD", []))]))

let%test _ = sec_dir_test_fail "SecTion .ttext \nadd"

let directive_suc = parse_test_suc pp_directive directive_p

let%test _ =
  directive_suc "SecTion .text \n\n   \nadd"
    (Directive
       [Section ("TEXT", [InDir (None, Instruction (Mnemonic "ADD", []))])] )

let%test _ =
  directive_suc "\nadd; comment"
    (Directive
       [Section ("TEXT", [InDir (None, Instruction (Mnemonic "ADD", []))])] )

let%test _ =
  directive_suc "section .text\nmov rax,0x001"
    (Directive
       [ Section
           ( "TEXT"
           , [InDir (None, Instruction (Mnemonic "MOV", [Reg "RAX"; Const 1L]))]
           ) ] )
