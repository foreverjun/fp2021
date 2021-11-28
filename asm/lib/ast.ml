(** The main type for asm AST *)

type directive_list = DirectiveDist of directive list

and directive = Directive of simple_seg_dir * in_seg_dir list option

and general_dir =
  | EquDir of id * expr
  | EqualDir of id * expr

and in_seg_dir = id option * in_segment_dir

and in_segment_dir =
  | Instruction of instruction
  | DataDir of data_dir
  | GeneralDir of general_dir

and instruction = mnemonic * expr list option

and data_dir = data_decl * init_value list

and init_value =
  | Expr of expr
  | Str of string
  | Dup of init_value list

and expr =
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Dev of expr * expr
  | Mod of expr * expr
  | Shl of expr * expr
  | Shr of expr * expr
  | Reg of string
  | Label of string
  | Const of int
[@@deriving show { with_path = false }]

and simple_seg_dir = SimpleSegDit
(* .CODE | .TEXT | .DATA  | .CONST *)

and data_decl = DataDecl
(* DB | DW | DD | DF | DQ | DT *)

and mnemonic = Mnemonic of string
(* commands *)

and id = Id of string
