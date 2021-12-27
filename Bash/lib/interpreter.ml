open Ast

(* -------------------- Helper module types and modules -------------------- *)

module type MonadFail = sig
  include Base.Monad.Infix

  val return : 'a -> 'a t
  val fail : string -> 'a t
end

(** Result as a monad with a fail function *)
module Result : MonadFail with type 'a t = ('a, string) result = struct
  type 'a t = ('a, string) result

  let return = Result.ok
  let ( >>= ) = Result.bind
  let ( >>| ) f g = f >>= fun x -> return (g x)
  let fail = Result.error
end

(** Map with string keys and pretty-printing for deriving *)
module SMap = struct
  include Map.Make (String)

  let pp pp_v ppf m =
    Format.fprintf ppf "@[[@[";
    iter (fun k v -> Format.fprintf ppf "@[\"%s\": %a@],@\n" k pp_v v) m;
    Format.fprintf ppf "@]]@]"
  ;;
end

(* -------------------- Interpreter -------------------- *)

(** Main interpreter module *)
module Eval (M : MonadFail) = struct
  open M

  (* -------------------- Environment -------------------- *)

  (** Container for values of variables *)
  type var_t =
    | Val of string (** Simple variable *)
    | IndArray of string list (** Indexed array *)
    | AssocArray of string SMap.t (** Associative array *)
  [@@deriving show { with_path = false }]

  (** Container for functions (takes arguments and produces contents of stdout, stderr, and return code) *)
  type fun_t = string list -> string * string * int
  [@@deriving show { with_path = false }]

  (** Environment containing variables available in the current scope *)
  type variables = var_t SMap.t [@@deriving show { with_path = false }]

  (** Environment containing functions defined in the current scope *)
  type functions = fun_t SMap.t [@@deriving show { with_path = false }]

  (** Complete environment *)
  type environment =
    { vars : variables
    ; funs : functions
    ; retcode : int
    }
  [@@deriving show { with_path = false }]

  (** All available environments with the first element as the innermost scope *)
  type environments = environment list [@@deriving show { with_path = false }]

  (** Searches for a variable with the given name in the provided environments *)
  let rec find_var (name : string) : environments -> var_t option = function
    | [] -> None
    | env :: tl ->
      (match SMap.find_opt name env.vars with
      | Some v -> Some v
      | None -> find_var name tl)
  ;;

  (** Searches for a function with the given name in the provided environments *)
  let rec find_fun (name : string) : environments -> fun_t option = function
    | [] -> None (* TODO: search for a script file *)
    | env :: tl ->
      (match SMap.find_opt name env.funs with
      | Some f -> Some f
      | None -> find_fun name tl)
  ;;

  (* -------------------- Evaluation -------------------- *)

  (** Evaluate variable *)
  let ev_var (envs : environments) : var -> string t =
    let list_find l i =
      match List.nth_opt l i with
      | None -> ""
      | Some v -> v
    in
    let map_find t i =
      match SMap.find_opt i t with
      | None -> ""
      | Some v -> v
    in
    function
    | SimpleVar name ->
      (match find_var name envs with
      | None -> return ""
      | Some (Val v) -> return v
      | Some (IndArray vs) -> return (list_find vs 0)
      | Some (AssocArray vs) -> return (map_find vs "0"))
    | Subscript (name, index) ->
      (match find_var name envs with
      | None -> return ""
      | Some (Val v) ->
        (match int_of_string_opt index with
        | None -> return v
        | Some i when i = 0 -> return v
        | Some _ -> return "")
      | Some (IndArray vs) ->
        (match int_of_string_opt index with
        | None -> return (list_find vs 0)
        | Some i -> return (list_find vs i))
      | Some (AssocArray vs) -> return (map_find vs index))
  ;;

  (** Evaluate arithmetic *)
  let ev_arithm envs =
    let rec ev_ari ?(c = fun _ _ -> true) ?(e = "") op l r =
      ev l >>= fun l -> ev r >>= fun r -> if c l r then return (op l r) else fail e
    and ev_log op l r = ev l >>= fun l -> ev r >>| fun r -> if op l r then 1 else 0
    and ev = function
      | Num n -> return n
      | Var x ->
        ev_var envs x
        >>| fun s ->
        (match int_of_string_opt s with
        | None -> 0
        | Some n -> n)
      | Plus (l, r) -> ev_ari ( + ) l r
      | Minus (l, r) -> ev_ari ( - ) l r
      | Mul (l, r) -> ev_ari ( * ) l r
      | Div (l, r) -> ev_ari ~c:(fun _ r -> r != 0) ~e:"Division by 0" ( / ) l r
      | Less (l, r) -> ev_log ( < ) l r
      | Greater (l, r) -> ev_log ( > ) l r
      | LessEq (l, r) -> ev_log ( <= ) l r
      | GreaterEq (l, r) -> ev_log ( >= ) l r
      | Equal (l, r) -> ev_log ( = ) l r
      | NEqual (l, r) -> ev_log ( <> ) l r
    in
    ev
  ;;

  (** Evaluate Bash script *)
  let ev_script = function
    | _ -> fail "Not implemented"
  ;;
end

(** Interprets the given Bash script AST as a Bash script *)
let interpret script =
  let open Eval (Result) in
  match ev_script script with
  | _ -> "Not implemented"
;;

(* ----------------------------------------------- *)
(* -------------------- Tests -------------------- *)
(* ----------------------------------------------- *)

open Eval (Result)

(* -------------------- Helper functions -------------------- *)

let succ_ev ?(envs = []) pp fmt ev ast exp =
  match ev envs ast with
  | Error e ->
    Printf.printf "Error: %s\n" e;
    false
  | Ok res when exp = res -> true
  | Ok res ->
    print_string "\n-------------------- Input --------------------\n";
    pp Format.std_formatter ast;
    Format.pp_print_flush Format.std_formatter ();
    print_string "\n----------------- Environments --------------------\n";
    pp_environments Format.std_formatter envs;
    Format.pp_print_flush Format.std_formatter ();
    print_string "\n------------------- Expected ------------------\n";
    print_string (fmt exp);
    print_string "\n-------------------- Actual -------------------\n";
    print_string (fmt res);
    print_string "\n-----------------------------------------------\n";
    flush stdout;
    false
;;

let fail_ev ?(envs = []) pp fmt ev ast exp =
  match ev envs ast with
  | Error e when exp = e -> true
  | Error e ->
    print_string "\n-------------------- Input --------------------\n";
    pp Format.std_formatter ast;
    Format.pp_print_flush Format.std_formatter ();
    print_string "\n----------------- Environments --------------------\n";
    pp_environments Format.std_formatter envs;
    Format.pp_print_flush Format.std_formatter ();
    print_string "\n--------------- Unexpected error ------------------\n";
    print_string e;
    print_string "\n-----------------------------------------------\n";
    flush stdout;
    false
  | Ok res ->
    print_string "\n-------------------- Input --------------------\n";
    pp Format.std_formatter ast;
    Format.pp_print_flush Format.std_formatter ();
    print_string "\n----------------- Environments --------------------\n";
    pp_environments Format.std_formatter envs;
    Format.pp_print_flush Format.std_formatter ();
    print_string "\n-------------------- Actual -------------------\n";
    print_string (fmt res);
    print_string "\n-----------------------------------------------\n";
    flush stdout;
    false
;;

(* -------------------- Variable -------------------- *)

let succ_ev_var ?(envs = []) = succ_ev ~envs pp_var Fun.id ev_var
let fail_ev_var ?(envs = []) = fail_ev ~envs pp_var Fun.id ev_var

let%test _ = succ_ev_var (SimpleVar "ABC") ""
let%test _ = succ_ev_var (Subscript ("ABC", "0")) ""

let%test _ =
  succ_ev_var
    ~envs:[ { vars = SMap.singleton "ABC" (Val "2"); funs = SMap.empty; retcode = 0 } ]
    (SimpleVar "ABC")
    "2"
;;

let%test _ =
  succ_ev_var
    ~envs:
      [ { vars = SMap.singleton "ABC" (IndArray [ "a"; "b"; "c" ])
        ; funs = SMap.empty
        ; retcode = 0
        }
      ]
    (SimpleVar "ABC")
    "a"
;;

let%test _ =
  succ_ev_var
    ~envs:
      [ { vars = SMap.singleton "ABC" (IndArray [ "a"; "b"; "c" ])
        ; funs = SMap.empty
        ; retcode = 0
        }
      ]
    (Subscript ("ABC", "1"))
    "b"
;;

let%test _ =
  succ_ev_var
    ~envs:
      [ { vars = SMap.singleton "ABC" (IndArray [ "a"; "b"; "c" ])
        ; funs = SMap.empty
        ; retcode = 0
        }
      ]
    (Subscript ("ABC", "3"))
    ""
;;

let%test _ =
  succ_ev_var
    ~envs:
      [ { vars =
            SMap.singleton
              "ABC"
              (AssocArray (SMap.of_seq (List.to_seq [ "a", "a1"; "b", "b1"; "0", "01" ])))
        ; funs = SMap.empty
        ; retcode = 0
        }
      ]
    (SimpleVar "ABC")
    "01"
;;

let%test _ =
  succ_ev_var
    ~envs:
      [ { vars =
            SMap.singleton
              "ABC"
              (AssocArray (SMap.of_seq (List.to_seq [ "a", "a1"; "b", "b1"; "0", "01" ])))
        ; funs = SMap.empty
        ; retcode = 0
        }
      ]
    (Subscript ("ABC", "b"))
    "b1"
;;

let%test _ =
  succ_ev_var
    ~envs:
      [ { vars =
            SMap.singleton
              "ABC"
              (AssocArray (SMap.of_seq (List.to_seq [ "a", "a1"; "b", "b1"; "0", "01" ])))
        ; funs = SMap.empty
        ; retcode = 0
        }
      ]
    (Subscript ("ABC", "!"))
    ""
;;

let%test _ =
  succ_ev_var
    ~envs:
      [ { vars = SMap.singleton "ABC" (Val "1"); funs = SMap.empty; retcode = 0 }
      ; { vars = SMap.singleton "ABC" (Val "2"); funs = SMap.empty; retcode = 0 }
      ]
    (SimpleVar "ABC")
    "1"
;;

(* -------------------- Arithmetic -------------------- *)

let succ_ev_arithm ?(envs = []) = succ_ev ~envs pp_arithm string_of_int ev_arithm
let fail_ev_arithm ?(envs = []) = fail_ev ~envs pp_arithm string_of_int ev_arithm

let%test _ = succ_ev_arithm (Plus (Num 1, Num 2)) 3
let%test _ = succ_ev_arithm (Div (Num 1, Num 3)) 0
let%test _ = succ_ev_arithm (Div (Num 2, Num 3)) 0
let%test _ = succ_ev_arithm (Div (NEqual (Num 1, Num 2), Greater (Num 3, Num 1))) 1
let%test _ = fail_ev_arithm (Div (Num 1, Num 0)) "Division by 0"
