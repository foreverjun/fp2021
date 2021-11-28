(** AST *)

type name =
  | Name of string (* Names of variables and functions; consist of letters, numbers, underscores, and begin with a letter or underscore *)
[@@deriving show { with_path = false }]

type var =
  | SimpleVar of name (* name *)
  | Subscript of name * string (* name[subscript] *)
[@@deriving show { with_path = false }]

type word =
  (*
  Tokens which are subject to expansions in the following cases:
  Redirection: BraceExp*, ParameterExp, CommandSubst, ArithmExp, WordSpl*, FilenameExp*, QuoteRem (error if expands to more than one word)
  For (with list): BraceExp*, ParameterExp, CommandSubst, ArithmExp, WordSpl*, FilenameExp*, QuoteRem
  Case: ParameterExp, CommandSubst, ArithmExp, QuoteRem
  Case item: ParameterExp, CommandSubst, ArithmExp
  Simple command: BraceExp*, ParameterExp, CommandSubst, ArithmExp, WordSpl*, FilenameExp*, QuoteRem
  Assignment: ParameterExp, CommandSubst, ArithmExp, QuoteRem
  * means that the expansion may produce more than one word
  *)
  | Word of string (* If none of the expansions below are applicable *)
  | ParamExp of param_exp (* Parameter expansion *)
  | CmdSubst of cmd (* $(command) *)
  | ArithmExp of arithm (* $((...)) *)
[@@deriving show { with_path = false }]

and param_exp =
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

and redir = Redir of int * redir_op * word (* descriptor + operator + word *)
[@@deriving show { with_path = false }]

and redir_op =
  | Redir_inp (* [n]<word *)
  | Redir_otp (* [n]>word *)
  | Append_otp (* [n]>>word *)
  | Dupl_inp (* [n]<&word *)
  | Dupl_otp (* [n]>&word *)
[@@deriving show { with_path = false }]

and script = Script of script_elem list [@@deriving show { with_path = false }]

and script_elem =
  | FuncDecl of func
  | Pipelines of pipeline_list
[@@deriving show { with_path = false }]

and func =
  | Func of name * compound * redir list (* [ function ] name () compound-command [redir] *)
[@@deriving show { with_path = false }]

and pipeline_list =
  | SinglePipeline of pipeline (* list containing a single pipeline *)
  | PipelineList of pipeline * pipeline_list_op * pipeline_list (* pipeline1 && pipeline2 or pipeline1 || pipeline2 *)
[@@deriving show { with_path = false }]

and pipeline_list_op =
  | And
  | Or
[@@deriving show { with_path = false }]

and pipeline =
  | Compound of bool * compound (* [ ! ] command *)
  | Pipeline of bool * compound * pipeline (* [ ! ] command1 | command2 [ | command3 ] *)
[@@deriving show { with_path = false }]

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
  | ListFor of name * word list * pipeline_list (* for name [ [ in [ word ... ] ] ; ] do list ; done *)
  | ExprFor of arithm * arithm * arithm * pipeline_list (* for (( expr1 ; expr2 ; expr3 )) ; do list ; done *)
[@@deriving show { with_path = false }]

and if_stmt =
  | SimpleIf of pipeline_list * pipeline_list (* if list; then list; fi *)
  | IfElse of pipeline_list * pipeline_list * pipeline_list (* if list; then list; else list; fi *)
[@@deriving show { with_path = false }]

and case_stmt =
  | CaseStmt of word * case_item list (* case word in [ case_item ] ... esac *)
[@@deriving show { with_path = false }]

and case_item =
  | CaseItem of word * word list * pipeline_list (* [(] pattern [ | pattern ] ... ) list ;; *)
[@@deriving show { with_path = false }]

and arithm =
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

and cmd =
  | Assignt of assignt * assignt list (* assignment [ other_assignments ] *)
  | Command of assignt list * word * word list (* [ assignments ] command [ parameters ] *)
[@@deriving show { with_path = false }]

and assignt =
  | SimpleAssignt of var * word option (* variable=[ value ] *)
  | CompoundAssignt of var * word list (* variable=(word1 ... wordn), if no words are provided, the array is not set *)
[@@deriving show { with_path = false }]
