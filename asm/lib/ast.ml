(** Mini Assembler AST. *)

(** The main type for assembler AST. *)
type directive = Directive of sec_dir list
[@@deriving show {with_path= false}]

(** [sec_dir] splits [directive] into sections, which begin with the keyword [SECTION] and the section type [.CODE], [.TEXT], [.DATA] or [.CONST]. *)
and sec_dir = Section of string * in_sec_dir list
[@@deriving show {with_path= false}]

(** [in_sec_dir] separates out the special assembler section elements: [EQU] and [EQUAL]. *)
and in_sec_dir =
  | InDir of id option * in_segment_dir
  | EquDir of id * expr
  | EqualDir of id * expr
[@@deriving show {with_path= false}]

(** [in_segment_dir] divides the section into data and code segments. *)
and in_segment_dir =
  | Instruction of mnemonic * expr list
  | DataDecl of string * init_value list
[@@deriving show {with_path= false}]

(** [init_value] contains the initial value of the expression or string.*)
and init_value = Expr of expr | Str of string | Dup of expr * string
[@@deriving show {with_path= false}]

(** [expr] contains the expression structure. *)
and expr =
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Mod of expr * expr
  | Shl of expr * expr
  | Shr of expr * expr
  | Reg of string
  | Label of string
  | Const of int64
[@@deriving show {with_path= false}]

(** [mnemonic] contains the name of the commands. *)
and mnemonic = Mnemonic of string [@@deriving show {with_path= false}]

(** [id] contains an assembler label. *)
and id = Id of string [@@deriving show {with_path= false}]

type simpl_dir = command list [@@deriving show {with_path= false}]

and command =
  | Arg0 of id option * mnemonic
  | Arg1 of id option * mnemonic * expr
  | Arg2 of id option * mnemonic * expr * expr
[@@deriving show {with_path= false}]
