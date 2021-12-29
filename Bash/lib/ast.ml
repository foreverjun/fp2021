(** AST *)

(** Name of a variable or a function *)
type name = string [@@deriving show { with_path = false }]

(** Variable reference *)
type var =
  | SimpleVar of name (** name *)
  | Subscript of name * string (** name\[subscript\] *)
[@@deriving show { with_path = false }]

(** Arithmetical expression *)
type arithm =
  | Num of int
  | Var of var
  | Plus of arithm * arithm
  | Minus of arithm * arithm
  | Mul of arithm * arithm
  | Div of arithm * arithm
  | Less of arithm * arithm
  | Greater of arithm * arithm
  | LessEq of arithm * arithm
  | GreaterEq of arithm * arithm
  | Equal of arithm * arithm
  | NEqual of arithm * arithm
[@@deriving show { with_path = false }]

(** Parameter expansion *)
type param_exp =
  | Param of var (** $name, $\{name\} *)
  | PosParam of int (** Positional parameters denoted with a number *)
  | Length of var (** $\{#name\} *)
  | Substring of var * arithm * arithm option (** $\{name:offset\[:length\]\} *)
  | CutMinBeg of var * string (** $\{name#pattern\} *)
  | CutMaxBeg of var * string (** $\{name##pattern\} *)
  | CutMinEnd of var * string (** $\{name%pattern\} *)
  | CutMaxEnd of var * string (** $\{name%%pattern\} *)
  | SubstOne of var * string * string (** $\{name/pattern\[/string\]\} *)
  | SubstAll of var * string * string (** $\{name//pattern\[/string\]\} *)
  | SubstBeg of var * string * string (** $\{name/#pattern\[/string\]\} *)
  | SubstEnd of var * string * string (** $\{name/%pattern\[/string\]\} *)
[@@deriving show { with_path = false }]

(** Token that may be subject to expansions *)
type word =
  (*
  Cases of expansions (here * means that the expansion may produce more than one word):
  Redirection: BraceExp*, ParameterExp, CommandSubst, ArithmExp, WordSpl*, FilenameExp*, QuoteRem (error if expands to more than one word)
  For (with list): BraceExp*, ParameterExp, CommandSubst, ArithmExp, WordSpl*, FilenameExp*, QuoteRem
  Case: ParameterExp, CommandSubst, ArithmExp, QuoteRem
  Case item: ParameterExp, CommandSubst, ArithmExp
  Simple command: BraceExp*, ParameterExp, CommandSubst, ArithmExp, WordSpl*, FilenameExp*, QuoteRem
  Assignment: ParameterExp, CommandSubst, ArithmExp, QuoteRem
  *)
  | BraceExp of word list (** Brace expansion *)
  | ParamExp of param_exp (** Parameter expansion *)
  | CmdSubst of cmd (** $(command) *)
  | ArithmExp of arithm (** $((...)) *)
  | FilenameExp of string (** A string that includes *, ? or \[ *)
  | Word of string (** If none of the expansions above are applicable *)
[@@deriving show { with_path = false }]

(** Simple command *)
and cmd =
  | Assignt of assignt * assignt list (** assignment \[ other_assignments \] *)
  | Command of assignt list * word * word list
      (** \[ assignments \] command \[ parameters \] *)
[@@deriving show { with_path = false }]

(** Assignment *)
and assignt =
  | SimpleAssignt of var * word option (** variable=\[ value \] *)
  | CompoundAssignt of var * word list
      (** variable=(word1 ... wordn), if no words are provided, the array is not set *)
[@@deriving show { with_path = false }]

(** Redirection *)
type redir =
  | RedirInp of int * word (** \[n\]<word *)
  | RedirOtp of int * word (** \[n\]>word *)
  | AppendOtp of int * word (** \[n\]>>word *)
  | DuplInp of int * word (** \[n\]<&word *)
  | DuplOtp of int * word (** \[n\]>&word *)
[@@deriving show { with_path = false }]

(** List of pipelines *)
type pipeline_list =
  | Pipeline of pipeline (** list containing a single pipeline *)
  | PipelineAndList of pipeline * pipeline_list (** pipeline && other-pipelines *)
  | PipelineOrList of pipeline * pipeline_list (** pipeline || other-pipelines *)
[@@deriving show { with_path = false }]

(** Pipeline in the form of \[ ! \] command1 \[ | command2 \[ | ... \] \] *)
and pipeline = bool * compound * compound list [@@deriving show { with_path = false }]

(** Compound command *)
and compound =
  | While of while_loop * redir list
  | ForList of for_list_loop * redir list
  | ForExpr of for_expr_loop * redir list
  | If of if_stmt * redir list
  | Case of case_stmt * redir list
  | ArithmExpr of arithm * redir list (** (( ... )) *)
  | SimpleCommand of cmd * redir list
[@@deriving show { with_path = false }]

(** While loop in the form of while list; do list; done *)
and while_loop = pipeline_list * pipeline_list [@@deriving show { with_path = false }]

(** For loop in the form of or name in \[ word ... \]; do list ; done *)
and for_list_loop = name * word list * pipeline_list
[@@deriving show { with_path = false }]

(** For loop in the form of for (( expr1 ; expr2 ; expr3 )) ; do list ; done *)
and for_expr_loop = arithm * arithm * arithm * pipeline_list
[@@deriving show { with_path = false }]

(** If statement in the form of if list; then list; \[ else list; \] fi *)
and if_stmt = pipeline_list * pipeline_list * pipeline_list option
[@@deriving show { with_path = false }]

(** Case statement in the form case word in \[ case_item \] ... esac *)
and case_stmt = word * case_item list [@@deriving show { with_path = false }]

(** An element of a case statement in the form \[(\] pattern \[ | pattern \] ... ) list ;; *)
and case_item = word * word list * pipeline_list [@@deriving show { with_path = false }]

(** Function in the form of function name \[()\] compound or name () compound *)
type func = name * compound [@@deriving show { with_path = false }]

(** AST root *)
type script =
  | Empty
  | Script of script_elem * script
[@@deriving show { with_path = false }]

(** A function declaration or a pipeline list *)
and script_elem =
  | Func of func
  | Pipelines of pipeline_list
[@@deriving show { with_path = false }]
