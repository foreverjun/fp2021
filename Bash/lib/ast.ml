(** AST *)

(** Name of a variable or a function *)
type name = Name of string [@@deriving show { with_path = false }]

(** Variable reference *)
type var =
  | SimpleVar of name (* name *)
  | Subscript of name * string (* name[subscript] *)
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
  | Param of var (* $name, ${name} *)
  | Length of var (* ${#name} *)
  | Substring of var * int * int (* ${name:offset[:length]} *)
  | CutMinBeg of var * string (* ${name#pattern} *)
  | CutMaxBeg of var * string (* ${name##pattern} *)
  | CutMinEnd of var * string (* ${name%pattern} *)
  | CutMaxEnd of var * string (* ${name%%pattern} *)
  | SubstOne of var * string * string (* ${name/pattern[/string]} *)
  | SubstAll of var * string * string (* ${name//pattern[/string]} *)
  | SubstBeg of var * string * string (* ${name/#pattern[/string]} *)
  | SubstEnd of var * string * string (* ${name/%pattern[/string]} *)
[@@deriving show { with_path = false }]

(** Token which may be subject to expansions *)
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
  | BraceExp of string list (* Brace expansion *)
  | ParamExp of param_exp (* Parameter expansion *)
  | CmdSubst of cmd (* $(command) *)
  | ArithmExp of arithm (* $((...)) *)
  | Word of string (* If none of the expansions above are applicable *)
[@@deriving show { with_path = false }]

(** Simple command *)
and cmd =
  | Assignt of assignt * assignt list (* assignment [ other_assignments ] *)
  | Command of assignt list * word * word list (* [ assignments ] command [ parameters ] *)
[@@deriving show { with_path = false }]

(** Assignment *)
and assignt =
  | SimpleAssignt of var * word option (* variable=[ value ] *)
  | CompoundAssignt of var * word list (* variable=(word1 ... wordn), if no words are provided, the array is not set *)
[@@deriving show { with_path = false }]

(** Redirection *)
type redir =
  | Redir_inp of int * word (* [n]<word *)
  | Redir_otp of int * word (* [n]>word *)
  | Append_otp of int * word (* [n]>>word *)
  | Dupl_inp of int * word (* [n]<&word *)
  | Dupl_otp of int * word (* [n]>&word *)
[@@deriving show { with_path = false }]

(** List of pipelines *)
type pipeline_list =
  | SinglePipeline of pipeline (* list containing a single pipeline *)
  | PipelineAndList of pipeline * pipeline_list (* pipeline && other-pipelines *)
  | PipelineOrList of pipeline * pipeline_list (* pipeline || other-pipelines *)
[@@deriving show { with_path = false }]

(** Pipeline *)
and pipeline =
  | Pipeline of bool * compound * compound list (* [ ! ] command1 [ | command2 [ | ... ] ] *)
[@@deriving show { with_path = false }]

(** Compound command *)
and compound =
  | While of while_loop * redir list
  | For of for_loop * redir list
  | If of if_stmt * redir list
  | Case of case_stmt * redir list
  | ArithmExpr of arithm * redir list (* (( ... )) *)
  | SimpleCommand of cmd * redir list
[@@deriving show { with_path = false }]

and while_loop =
  | WhileLoop of pipeline_list * pipeline_list (* while list; do list; done *)
[@@deriving show { with_path = false }]

and for_loop =
  | ListFor of name * word list * pipeline_list (* for name in [ word ... ]; do list ; done *)
  | ExprFor of arithm * arithm * arithm * pipeline_list (* for (( expr1 ; expr2 ; expr3 )) ; do list ; done *)
[@@deriving show { with_path = false }]

and if_stmt =
  | IfStmt of pipeline_list * pipeline_list * pipeline_list option (* if list; then list; [ else list; ] fi *)
[@@deriving show { with_path = false }]

and case_stmt =
  | CaseStmt of word * case_item list (* case word in [ case_item ] ... esac *)
[@@deriving show { with_path = false }]

and case_item =
  | CaseItem of word * word list * pipeline_list (* [(] pattern [ | pattern ] ... ) list ;; *)
[@@deriving show { with_path = false }]

(** Function *)
type func =
  | Func of name * compound * redir list (* [ function ] name () compound-command [redir] *)
[@@deriving show { with_path = false }]

(** AST root *)
type script = Script of script_elem list [@@deriving show { with_path = false }]

and script_elem =
  | FuncDecl of func
  | Pipelines of pipeline_list
[@@deriving show { with_path = false }]
