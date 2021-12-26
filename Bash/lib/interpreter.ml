open Ast

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
end

module type MONAD_FAIL = sig
  include MONAD

  val fail : string -> 'a t
end

module Result : MONAD_FAIL with type 'a t = ('a, string) result = struct
  type 'a t = ('a, string) result

  let return = Result.ok
  let ( >>= ) = Result.bind
  let ( >>| ) f g = f >>= fun x -> return (g x)
  let fail = Result.error
end

module Eval (M : MONAD_FAIL) = struct
  open M

  (* -------------------- Environment -------------------- *)

  (** Container for values of variables *)
  type var_t =
    | Val of string (** Simple variable *)
    | IndArray of string list (** Indexed array *)
    | AssocArray of (string, string) Hashtbl.t (** Associative array *)

  (** Container for functions (takes arguments and produces contents of stdout, stderr, and return code) *)
  type fun_t = string list -> string * string * int

  (** Environment containing variables available in the current scope *)
  type variables = (string, var_t) Hashtbl.t

  (** Environment containing functions defined in the current scope *)
  type functions = (string, fun_t) Hashtbl.t

  (** Complete environment *)
  type environment =
    { vars : variables
    ; funs : functions
    ; retcode : int
    }

  (** All available environments with the first element as the innermost scope *)
  type environments = environment list

  (** Searches for a variable with the given name in the provided environments *)
  let rec find_var (name : string) : environments -> var_t option = function
    | [] -> None
    | env :: tl ->
      (match Hashtbl.find_opt env.vars name with
      | Some v -> Some v
      | None -> find_var name tl)
  ;;

  (** Searches for a function with the given name in the provided environments *)
  let rec find_fun (name : string) : environments -> fun_t option = function
    | [] -> None (* TODO: search for a script file *)
    | env :: tl ->
      (match Hashtbl.find_opt env.funs name with
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
    let htbl_find t i =
      match Hashtbl.find_opt t i with
      | None -> ""
      | Some v -> v
    in
    function
    | SimpleVar (Name name) ->
      (match find_var name envs with
      | None -> return ""
      | Some (Val v) -> return v
      | Some (IndArray vs) -> return (list_find vs 0)
      | Some (AssocArray vs) -> return (htbl_find vs "0"))
    | Subscript (Name name, index) ->
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
      | Some (AssocArray vs) -> return (htbl_find vs index))
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
  | _ -> failwith "Not implemented"
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
    print_string "\n------------------- Expected ------------------\n";
    print_string (fmt exp);
    print_string "\n-------------------- Actual -------------------\n";
    print_string (fmt res);
    print_string "\n-----------------------------------------------\n";
    false
;;

let fail_ev ?(envs = []) pp fmt ev ast exp =
  match ev envs ast with
  | Error e when exp = e -> true
  | Error e ->
    print_string "\n-------------------- Input --------------------\n";
    pp Format.std_formatter ast;
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
    print_string "\n-------------------- Actual -------------------\n";
    print_string (fmt res);
    print_string "\n-----------------------------------------------\n";
    flush stdout;
    false
;;

(* -------------------- Arithmetic -------------------- *)

let succ_ev_arithm ?(envs = []) = succ_ev ~envs pp_arithm string_of_int ev_arithm
let fail_ev_arithm ?(envs = []) = fail_ev ~envs pp_arithm string_of_int ev_arithm

let%test _ = succ_ev_arithm (Plus (Num 1, Num 2)) 3
let%test _ = succ_ev_arithm (Div (Num 1, Num 3)) 0
let%test _ = succ_ev_arithm (Div (Num 2, Num 3)) 0
let%test _ = succ_ev_arithm (Div (NEqual (Num 1, Num 2), Greater (Num 3, Num 1))) 1
let%test _ = fail_ev_arithm (Div (Num 1, Num 0)) "Division by 0"
