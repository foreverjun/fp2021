(** Variable reference in the form of [name\[subscript\]] *)
type var = string * string [@@deriving show { with_path = false }]

(** Arithmetical expression *)
type arithm =
  | Num of int
  | Var of var
  | ArithmAssignt of var * arithm
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
  | Param of var (** [$name], [${name}] *)
  | Length of var (** [${#name}] *)
  | Substring of var * arithm * arithm option (** [${name:offset\[:length\]}] *)
  | CutMinBeg of var * string (** [${name#pattern}] *)
  | CutMaxBeg of var * string (** [${name##pattern}] *)
  | CutMinEnd of var * string (** [${name%pattern}] *)
  | CutMaxEnd of var * string (** [${name%%pattern}] *)
  | SubstOne of var * string * string (** [${name/pattern\[/string\]}] *)
  | SubstAll of var * string * string (** [${name//pattern\[/string\]}] *)
  | SubstBeg of var * string * string (** [${name/#pattern\[/string\]}] *)
  | SubstEnd of var * string * string (** [${name/%pattern\[/string\]}] *)
[@@deriving show { with_path = false }]

(** Token that may be subject to expansions (here * means that the expansion may produce more than one word):
  Redirection: BraceExp*, ParameterExp, CommandSubst, ArithmExp, WordSpl*, FilenameExp*, QuoteRem (error if expands to more than one word)
  For (with list): BraceExp*, ParameterExp, CommandSubst, ArithmExp, WordSpl*, FilenameExp*, QuoteRem
  Case: ParameterExp, CommandSubst, ArithmExp, QuoteRem
  Case item: ParameterExp, CommandSubst, ArithmExp
  Simple command: BraceExp*, ParameterExp, CommandSubst, ArithmExp, WordSpl*, FilenameExp*, QuoteRem
  Assignment: ParameterExp, CommandSubst, ArithmExp, QuoteRem
*)
type word =
  | DoubleQuotes of word list (** A string that was quoted with [""] *)
  | BraceExp of word list (** Brace expansion *)
  | ParamExp of param_exp (** Parameter expansion *)
  | CmdSubst of cmd (** [$(command)] *)
  | ArithmExp of arithm (** [$((...))] *)
  | FilenameExp of string (** A string that includes [*], [?] or [\[] *)
  | Word of string (** If none of the expansions above are applicable or [''] were used *)
[@@deriving show { with_path = false }]

(** Assignment (differs from the original Bash not to depend on the declare built-in) *)
and assignt =
  | SimpleAssignt of var * word (** [variable=\[ value \]] *)
  | IndArrAssignt of string * word list
      (** [name=(word1 word2 ...)], if no words are provided, the indexed array is not set *)
  | AssocArrAssignt of string * (string * word) list
      (** [name=(key1=value1 key2=value2 ...)], if no pairs are provided, the array is not set *)
[@@deriving show { with_path = false }]

(** Redirection *)
and redir =
  | RedirInp of int * word (** [\[n\]<word] *)
  | RedirOtp of int * word (** [\[n\]>word] *)
  | AppendOtp of int * word (** [\[n\]>>word] *)
  | DuplInp of int * word (** [\[n\]<&word] *)
  | DuplOtp of int * word (** [\[n\]>&word] *)
[@@deriving show { with_path = false }]

(** Compound command *)
and compound =
  | Group of pipe_list list (** \{ ... \} *)
  | While of pipe_list * pipe_list (** [while list; do list; done] *)
  | ForList of string * word list * pipe_list
      (** [name in \[ word ... \]; do list ; done] *)
  | ForExpr of arithm * arithm * arithm * pipe_list
      (** [for (( expr1 ; expr2 ; expr3 )) ; do list ; done] *)
  | If of pipe_list * pipe_list * pipe_list option
      (** [if list; then list; \[ else list; \] fi] *)
  | Case of word * (word list * pipe_list) list
      (** [case word in \[ [\[(\] pattern \[ | pattern \] ... ) list ;;] \] ... esac] *)
  | ArithmExpr of arithm (** [(( ... ))] *)
[@@deriving show { with_path = false }]

(** Simple or compound command with redirections *)
and cmd =
  | Simple of assignt list * word list * redir list
  | Compound of compound * redir list

(** Pipeline in the form of [\[ ! \] command1 \[ | command2 \[ | ... \] \]] *)
and pipe = bool * cmd * cmd list [@@deriving show { with_path = false }]

(** List of pipelines *)
and pipe_list =
  | Pipe of pipe (** list containing a single pipeline *)
  | PipeAndList of pipe * pipe_list (** [pipeline && other-pipelines] *)
  | PipeOrList of pipe * pipe_list (** [pipeline || other-pipelines] *)
[@@deriving show { with_path = false }]

(** Function definition *)
type func = string * compound * redir list [@@deriving show { with_path = false }]

(** A function declaration or a command *)
type script_elem =
  | Func of func
  | Pipes of pipe_list
[@@deriving show { with_path = false }]

(** AST root *)
type script = script_elem list [@@deriving show { with_path = false }]
