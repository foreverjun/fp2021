(** Parses the given string as a Bash script *)
val parse : string -> (Ast.script, string) result

(** Splits the given string on tokens separated by blanks and newlines *)
val split_words : string -> (string list, string) result
