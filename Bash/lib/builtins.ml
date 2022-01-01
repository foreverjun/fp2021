type t = string list -> Unix.file_descr -> Unix.file_descr -> Unix.file_descr -> int

let echo args _ stdout _ =
  let otp = String.concat " " args in
  let len = String.length otp in
  if Unix.write_substring stdout otp 0 len <> len then 1 else 0
;;

let find : string -> t option = function
  | "echo" -> Some echo
  | _ -> None
;;
