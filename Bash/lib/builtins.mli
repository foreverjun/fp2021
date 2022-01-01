(** BuiltIn functions in the form of [cla -> stdin -> stdout -> stderr -> retcode] *)
type t = string list -> Unix.file_descr -> Unix.file_descr -> Unix.file_descr -> int

(** [find s] returns builtin with the name [s] if such builtin exists *)
val find : string -> t option
