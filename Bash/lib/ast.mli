(** AST *)

type name = Name of string
(* names of variables and functions; consist of letters, numbers, underscores, and begin with a letter or underscore *)

type word = Word of string * expansion list
(* tokens which are subject to expansions *)

and expansion =
  | BraceExp
  | ParameterExp
  | CommandSubst
  | ArithmExp
  | WordSpl
  | FilenameExp
  | QuoteRem
(*
Redirection: all (error if expands to more than one word)
For (with list): all
Case: ParameterExp, CommandSubst, ArithmExp, QuoteRem
Case item: ParameterExp, CommandSubst, ArithmExp
Simple command: all
Assignment: ParameterExp, CommandSubst, ArithmExp, QuoteRem
*)

type redir = Redir of int * redir_op * word
(* descriptor + operator + word *)

and redir_op =
  | Redir_inp (* [n]<word *)
  | Redir_otp (* [n]>word *)
  | Append_otp (* [n]>>word *)
  | Dupl_inp (* [n]<&word *)
  | Dupl_otp
(* [n]>&word *)

type script = Script of script_elem list

and script_elem =
  | FuncDecl of func
  | Pipelines of pipeline_list

and func = Func of name * compound * redir list
(* [ function ] name () compound-command [redir] *)

and pipeline_list =
  | SinglePipeline of pipeline (* list containing a single pipeline *)
  | PipelineList of pipeline * pipeline_list_op * pipeline_list
(* pipeline1 && pipeline2 or pipeline1 || pipeline2 *)

and pipeline_list_op =
  | And
  | Or

and pipeline =
  | Compound of bool * compound (* [ ! ] command *)
  | Pipeline of bool * compound * pipeline
(* [ ! ] command1 | command2 [ | command3 ] *)

and compound =
  | While of while_loop * redir list
  | For of for_loop * redir list
  | If of if_stmt * redir list
  | Case of case_stmt * redir list
  | ArifmExpr of arithm * redir list
  | SimpleCommand of cmd * redir list

and while_loop = WhileLoop of pipeline_list * pipeline_list
(* while list; do list; done *)

and for_loop =
  | ListFor of name * word list * pipeline_list (* for name [ [ in [ word ... ] ] ; ] do list ; done *)
  | ExprFor of arithm * arithm * arithm * pipeline_list
(* for (( expr1 ; expr2 ; expr3 )) ; do list ; done *)

(* elif is planned to be constructed as a sequence of if-else statements *)
and if_stmt =
  | SimpleIf of pipeline_list * pipeline_list (* if list; then list; fi *)
  | IfElse of pipeline_list * pipeline_list * pipeline_list
(* if list; then list; else list; fi *)

and case_stmt = CaseStmt of word * case_item list
(* case word in [ case_item ] ... esac *)

and case_item = CaseItem of word * word list * pipeline_list
(* [(] pattern [ | pattern ] ... ) list ;; *)

(* TODO: make clear if this amount of operators is enough *)
and arithm =
  (* (( ... )) *)
  | Num of int
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

and cmd =
  | Assignt of assignt * assignt list (* assignment [ other_assignments ] *)
  | Command of assignt list * word * word list
(* [ assignments ] command [ parameters ] *)

and assignt = AssigntStmt of name * word option
(* name=[ value ] *)
