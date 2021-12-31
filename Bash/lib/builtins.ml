type t = string list -> string -> string * string * string * int

let echo (args : string list) (stdin : string) = stdin, String.concat " " args, "", 0

let find : string -> t option = function
  | "echo" -> Some echo
  | _ -> None
;;
