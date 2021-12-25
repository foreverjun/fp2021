(** Parses the given string as a Bash script *)
val parse : string -> (Ast.script, string) result
