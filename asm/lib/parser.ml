(* TODO: implement parser here *)
open Angstrom
open Ast

let parse p s = parse_string ~consume:All p s

(* ___________________________________________________________________ Spaces and EOF *)
let is_space = function
  | '\t' | '\x11' | '\x12' | '\r' | '\x26' | ' ' -> true
  | _ -> false
;;

let spaces = skip_while is_space

let eol =
  let is_eol c = c = '\n' in
  char ';' *> skip_while (fun c -> not (is_eol c)) *> skip is_eol <|> skip is_eol
;;

let trim x = spaces *> x <* spaces

(* _____________________________________________________________________________ Expr *)

let parens p = trim (char '(' *> trim p <* char ')')
let add = char '+' *> return (fun x y -> Add (x, y))
let sub = char '-' *> return (fun x y -> Sub (x, y))
let mul = char '*' *> return (fun x y -> Mul (x, y))
let div = char '/' *> return (fun x y -> Dev (x, y))
let mmod = char '%' *> return (fun x y -> Mod (x, y))
let shl = string "<<" *> return (fun x y -> Shl (x, y))
let shr = string ">>" *> return (fun x y -> Shr (x, y))

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let hexNumber =
  option "" (string "+" <|> string "-")
  >>= fun sign ->
  spaces
  *> string "0x"
  *> option
       "0"
       (take_while1 (function
           | '0' .. '9' -> true
           | 'A' .. 'F' -> true
           | 'a' .. 'f' -> true
           | _ -> false))
  >>| fun s -> int_of_string (sign ^ "0x" ^ s)
;;

let decNumber =
  option "" (string "+" <|> string "-")
  >>= fun sign -> spaces *> take_while1 is_digit >>| fun s -> int_of_string (sign ^ s)
;;

let digit = hexNumber <|> decNumber >>| fun x -> Const x
(* let quote s = char '"' *> s <* char '"' <|> char '\'' *> s <* char '\'' >>| fun s -> Str s *)

let is_register = function
  (* 8 bit *)
  | "AH"
  | "AL"
  | "BH"
  | "BL"
  | "CH"
  | "CL"
  | "DH"
  | "DL"
  (* 16 bit *)
  | "AX"
  | "BX"
  | "CX"
  | "DX"
  (* 32 bit *)
  (* General-purpose *)
  | "EAX"
  | "EBX"
  | "ECX"
  | "EDX"
  | "ESI"
  | "EDI"
  (* Stack pointer *)
  | "ESP"
  (* Base pointer *)
  | "EBP"
  (* 64 bit *)
  | "RAX"
  | "RBX"
  | "RCX"
  | "RDX"
  | "RSP"
  | "RBP"
  | "RSI"
  | "RDI"
  (* 128 bit SSE *)
  | "XMM0"
  | "XMM1"
  | "XMM2"
  | "XMM3"
  | "XMM4"
  | "XMM5"
  | "XMM6"
  | "XMM7" -> true
  | _ -> false
;;

let id =
  let check_fst = function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' | '@' | '$' | '?' -> true
    | _ -> false
  in
  let check_inner c = check_fst c || is_digit c in
  satisfy check_fst
  >>= fun fst -> take_while1 check_inner >>| fun inner -> Char.escaped fst ^ inner
;;

let label =
  id
  >>| fun s ->
  if is_register (String.uppercase s) then Reg (String.uppercase s) else Label s
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let expr_p =
  fix (fun expr ->
      let factor = parens expr <|> trim digit <|> trim label in
      let term = trim (chainl1 factor (mul <|> div <|> shr <|> shl <|> mmod)) in
      trim (chainl1 term (add <|> sub)))
;;

(* ____________________________________________________________________________ Tests *)
