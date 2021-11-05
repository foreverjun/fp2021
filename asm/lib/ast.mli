(** The main type for asm AST *)
type script = 
| start * section_data * section_text             (* global _start   section .data   section .text *)
| start * section_text                            (* global _start   section .text *)

and start = string                                (* public _start *)

and section_data = var list                       (* section .data *)

and var = string * string * string list

and section_text = text_elem list                 (* section .text *)

and text_elem = 
| var
| func

and func = string * command_line list

and command_line = command * string list

and command = string
