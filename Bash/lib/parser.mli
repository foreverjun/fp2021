(** Parses the given string as a Bash script *)
val parse_result : string -> (Ast.script, string) result

(** Creates and returns a parser [state] for parsing a Bash script *)
val make_state : unit -> Ast.script Angstrom.Buffered.state
