(** Mini Assembler AST. *)

(** [id] contains an assembler label. *)
type id = Id of string [@@deriving show {with_path = false}]

(** [mnemonic] contains the name of the commands. *)
type mnemonic = Mnemonic of string [@@deriving show {with_path = false}]

(** [expr] contains the expression structure. *)
type expr =
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
[@@deriving show {with_path = false}]

(** [init_value] contains the initial value of the expression or string.*)
type init_value = Expr of expr | Str of string | Dup of expr * string
[@@deriving show {with_path = false}]

(** [command] contains the assembler instruction and the allowed number of arguments. *)
type command =
  | Arg0 of mnemonic
  | Arg1 of mnemonic * expr
  | Arg2 of mnemonic * expr * expr
[@@deriving show {with_path = false}]

(** [in_sec_dir] divides the section into data and code segments. *)
type in_sec_dir =
  | Instr of id option * command
  | DataDecl of id option * string * init_value list
  | EquDir of id * expr
  | EqualDir of id * expr
[@@deriving show {with_path = false}]

(** [sec_dir] splits [directive] into sections, which begin with the keyword [SECTION] and the section type [.CODE], [.TEXT], [.DATA] or [.CONST]. *)
type sec_dir = Code of in_sec_dir list | Data of in_sec_dir list
[@@deriving show {with_path = false}]

(** The main type for assembler AST. *)
type directive = Directive of sec_dir list
[@@deriving show {with_path = false}]

(** [simpl_dir] contains a list of all commands for the interpreter. The list is generated at the interpreter. *)
type simpl_dir = (id option * command) list
[@@deriving show {with_path = false}]
