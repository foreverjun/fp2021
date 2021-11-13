(** The main type for asm AST *)
type script = 
| WithData of start_code * section_data * section_text             (* global _start   section .data   section .text *)
| WithoutData of start_code * section_text                            (* global _start   section .text *)

and start_code = string                                (* public _start *)

and section_data = var list                       (* section .data *)

and var = string * string * string list

and section_text = text_elem list                 (* section .text *)

and text_elem = 
| Var of var
| Func of func

and func = string * command_line list

and command_line = command * string list

and command = string
