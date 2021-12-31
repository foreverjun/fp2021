(** BuiltIn functions in the form of [cla -> stdin -> (stdin, stdout, stderr, retcode)] *)
type t = string list -> string -> string * string * string * int

(** [find s] returns builtin with the name [s] if such builtin exists *)
val find : string -> t option
