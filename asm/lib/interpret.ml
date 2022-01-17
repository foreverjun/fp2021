open Ast
open Int64

(** Infix monad with an error function. *)
module type MonadError = sig
  include Base.Monad.Infix

  val return : 'a -> 'a t
  val error : string -> 'a t
end

(** Result as a monad-error. *)
module Result : MonadError with type 'a t = ('a, string) result = struct
  type 'a t = ('a, string) Result.t

  let ( >>= ) = Result.bind
  let return = Result.ok
  let ( >>| ) f g = f >>= fun x -> return (g x)
  let error = Result.error
end

(** Map with string key and print function. *)
module MapString = struct
  include Map.Make (String)

  let of_list l = of_seq (List.to_seq l)

  let pp pp_v ppf m =
    Format.fprintf ppf "@[[@[";
    iter (fun k v -> Format.fprintf ppf "@[\"%s\": %a@],@\n" k pp_v v) m;
    Format.fprintf ppf "@]]@]"
end

(** Main interpreter module. *)
module Eval (M : MonadError) = struct
  open M

  (** Container for variables. *)
  type var_t =
    (* registers and magic variables "0retcode", "0jump", "0cond" *)
    (* "0retcode" - the return code that appears after the exit system call. *)
    (* "0cond" - stores the result of the CMP command *)
    | R64 of int64
    | RSSE of string (* 128 bit *)
    (* constants and magic variable "0jump" *)
    (* "0jump" - stores the jump label *)
    | Ls of string
    (* labels whose call will cause an error *)
    | Er
  [@@deriving show {with_path = false}]

  (** Enviroment variables. *)
  type env = var_t MapString.t [@@deriving show {with_path = false}]

  (** [modulo x y] returns the positive remainder of the [Int64] division. *)
  let modulo x y =
    let result = rem x y in
    if result >= 0L then result else add result y

  (** [string_to_int64 s] takes a string [s], interprets it as a bit little-endian number and returns it as [Int64]. *)
  let string_to_int64 s =
    let rec helper acc n = function
      | "" -> acc
      | s when String.length s = 1 ->
          let c = of_int (Char.code s.[0]) in
          let res =
            add acc (shift_left (logand 0x7FL c) n)
            (* 0x7F = 0111_1111 i.e. remove the sign bit *) in
          if logand 0x80L (* or 0b1000_0000 *) c = 0L then res else neg res
      | s ->
          helper
            (add (shift_left (of_int (Char.code s.[0])) n) acc)
            (n + 8)
            (String.sub s 1 (String.length s - 1)) in
    helper 0L 0 s

  (** [int64_to_string i] takes an integer [i], interprets it as a hexadecimal number, makes and returns a little-endian string. *)
  let int64_to_string i =
    let rec helper acc = function
      | k when k = 0L -> acc
      | k when k = -1L -> (
        match String.length acc with
        | l when l >= 8 ->
            Printf.sprintf "%c%s"
              (Char.chr (Int.logor 0x80 (Char.code acc.[l - 1])))
              (String.sub acc 1 (l - 1))
        | l -> Printf.sprintf "%s%s\x80" acc (String.make (7 - l) '\x00') )
      | k ->
          let c =
            Printf.sprintf "%s%c" acc (Char.chr (to_int (modulo k 256L))) in
          helper c (shift_right_logical k 8) in
    helper "" i

  (** [ev e f] counts arithmetic operations, takes an [expr] and [f] that decides to process registers [Ast.Reg] and labels [Ast.Label]. *)
  let rec ev e f = function
    | Const n -> return n
    | Add (l, r) -> ev e f l >>= fun l -> ev e f r >>= fun r -> return (add l r)
    | Sub (l, r) -> ev e f l >>= fun l -> ev e f r >>= fun r -> return (sub l r)
    | Mul (l, r) -> ev e f l >>= fun l -> ev e f r >>= fun r -> return (mul l r)
    | Div (l, r) ->
        ev e f r
        >>= fun r ->
        if r = 0L then error "division by zero"
        else ev e f l >>= fun l -> return (div l r)
    | Mod (l, r) ->
        ev e f r
        >>= fun r ->
        if r = 0L then error "division by zero"
        else ev e f l >>= fun l -> return (modulo l r)
    | Shl (l, r) ->
        ev e f r
        >>= fun r ->
        if r < 0L then error "shift by a negative value"
        else ev e f l >>= fun l -> return (shift_left l (to_int r mod 64))
    | Shr (l, r) ->
        ev e f r
        >>= fun r ->
        if r < 0L then error "shift by a negative value"
        else
          ev e f l >>= fun l -> return (shift_right_logical l (to_int r mod 64))
    | rl -> f rl

  (** [prepr env dir] accepts the [environment] and [Ast.Directive], returns the environment filled with the labels found and the simplified directory with the instructions [Ast.simpl_dir]. *)
  let prepr env =
    let pr_expr env =
      let f e = function
        | Reg _ -> error "expression is not simple or relocatable"
        | Label l -> (
          match MapString.find_opt l e with
          | Some (Ls s) -> return (string_to_int64 s)
          | Some Er -> error "byte data exceeds bounds"
          | _ ->
              error "relocation truncated to fit: R_X86_64_8 against `.data\'" )
        | _ -> error "fatal error: other cases should be handled in ev" in
      function
      | Label l -> (
        match MapString.find_opt l env with
        | Some (Ls s) -> return (Ls s)
        | Some Er -> error "byte data exceeds bounds"
        | _ -> error "relocation truncated to fit: R_X86_64_8 against `.data\'"
        )
      | Reg _ -> error "expression is not simple or relocatable"
      | e -> ev env (f env) e >>= fun i -> return (Ls (int64_to_string i)) in
    let str_to_lb s = return (Ls s) in
    let env_add id v env =
      match MapString.find_opt id env with
      | None -> return (MapString.add id v env)
      | Some _ ->
          error (Printf.sprintf "label `%s\' inconsistently redefined" id) in
    let env_set id v env = return (MapString.add id v env) in
    let rec pr_init_val env acc =
      let helper env = function
        | Str s -> return (Ls (acc ^ s))
        | Expr e -> pr_expr env e
        | Dup (e, s) -> (
            pr_expr env e
            >>=
            let rec helper s acc d =
              if d = 0L then return acc else helper s (acc ^ s) (sub d 1L) in
            function
            | Ls k -> helper s "" (string_to_int64 k) >>| fun k -> Ls k
            | _ -> error "fatal error: pr_expr should return Ls" ) in
      function
      | [] -> (
        match acc with
        | "" -> error "no operand for data declaration"
        | _ -> return acc )
      | hd :: tl -> (
          helper env hd
          >>= function
          | Ls s -> pr_init_val env (acc ^ s) tl
          | _ -> error "fatal error: helper should return Ls" ) in
    let rec pr_data env =
      let pr_inst env = function
        | Instr (None, _) | DataDecl (None, _, _) -> return env
        | Instr (Some (Id id), _) -> env_add id Er env
        | DataDecl (Some (Id id), _, l) ->
            pr_init_val env "" l >>= str_to_lb >>= fun v -> env_add id v env
        | EquDir (Id id, e) -> pr_expr env e >>= fun v -> env_add id v env
        | EqualDir (Id id, e) -> pr_expr env e >>= fun v -> env_set id v env
      in
      function
      | [] -> return env
      | hd :: tl -> pr_inst env hd >>= fun env -> pr_data env tl in
    let rec pr_code env acc4 =
      let pr_inst env = function
        | DataDecl (None, _, _) -> return (env, [])
        | DataDecl (Some (Id id), _, l) ->
            pr_init_val env "" l >>= str_to_lb
            >>= fun v -> env_add id v env >>= fun env -> return (env, [])
        | EquDir (Id id, e) ->
            pr_expr env e
            >>= fun v -> env_add id v env >>= fun env -> return (env, [])
        | EqualDir (Id id, e) ->
            pr_expr env e
            >>= fun v -> env_set id v env >>= fun env -> return (env, [])
        | Instr (id, cmd) -> return (env, [(id, cmd)]) in
      function
      | [] -> return (env, acc4)
      | hd :: tl -> pr_inst env hd >>= fun (env, l) -> pr_code env (acc4 @ l) tl
    in
    let rec pr_sec_dir env acc =
      let helper env = function
        | Data l -> pr_data env l >>| fun env -> (env, [])
        | Code l -> pr_code env [] l >>| fun (env, new_acc) -> (env, new_acc)
      in
      function
      | [] -> return (env, acc)
      | hd :: tl ->
          helper env hd
          >>= fun (env, new_acc) -> pr_sec_dir env (acc @ new_acc) tl in
    fun (Directive list) -> pr_sec_dir env [] list

  (** List of all registers. *)
  let reg_list =
    [ ("RAX", R64 0L); ("RBX", R64 0L); ("RCX", R64 0L); ("RDX", R64 0L)
    ; ("RSP", R64 0L); ("RBP", R64 0L); ("RSI", R64 0L); ("RDI", R64 0L)
    ; ("XMM0", RSSE "\x00"); ("XMM1", RSSE "\x00"); ("XMM2", RSSE "\x00")
    ; ("XMM3", RSSE "\x00"); ("XMM4", RSSE "\x00"); ("XMM5", RSSE "\x00")
    ; ("XMM6", RSSE "\x00"); ("XMM7", RSSE "\x00"); ("0cond", R64 0L)
    ; ("0jump", Ls "") ]

  let reg8 = function
    | "AH" | "AL" | "BH" | "BL" | "CH" | "CL" | "DH" | "DL" -> true
    | _ -> false

  let reg8L = function "AL" | "BL" | "CL" | "DL" -> true | _ -> false
  let reg16 = function "AX" | "BX" | "CX" | "DX" -> true | _ -> false

  let reg32 = function
    | "EAX" | "EBX" | "ECX" | "EDX" | "ESI" | "EDI" | "ESP" | "EBP" -> true
    | _ -> false

  let reg64 = function
    | "RAX" | "RBX" | "RCX" | "RDX" | "RSP" | "RBP" | "RSI" | "RDI" -> true
    | _ -> false

  let regSSE = function
    | "XMM0" | "XMM1" | "XMM2" | "XMM3" | "XMM4" | "XMM5" | "XMM6" | "XMM7" ->
        true
    | _ -> false

  (** [interpret (env, list)] executes commands from a list in this environment. *)
  let interpret (env, list) =
    let f e =
      let take_full_reg = function
        | s when reg8 s -> return (Printf.sprintf "R%cX" s.[1])
        | s when reg16 s -> return (Printf.sprintf "R%s" s)
        | s when reg32 s -> return (Printf.sprintf "R%c%c" s.[1] s.[2])
        | s when reg64 s -> return s
        | _ -> error "invalid combination of opcode and operands" in
      function
      | Reg r -> (
          take_full_reg r
          >>= fun r ->
          match MapString.find r e with
          | R64 i -> return i
          | RSSE _ -> error "invalid combination of opcode and operands"
          | _ -> error "fatal error: register can only be R64 or RSSE" )
      | Label l -> (
        match MapString.find_opt l e with
        | Some (Ls s) -> return (string_to_int64 s)
        | Some Er -> error "byte data exceeds bounds"
        | _ -> error (Printf.sprintf "symbol `%s\' not defined" l) )
      | _ -> error "fatal error: other cases should be handled in ev" in
    let find_reg64 env r =
      return (MapString.find r env)
      >>= function
      | R64 i -> return i
      | _ -> error "falal error: register 64 can only be R64" in
    let change_reg64 env reg foo =
      let do_offset ot nt offs len =
        let os =
          let h = int64_to_string ot in
          h ^ String.make (8 - String.length h) '\x00' in
        let ns =
          let h = int64_to_string nt in
          h ^ String.make (8 - String.length h) '\x00' in
        Printf.sprintf "%s%s%s" (String.sub os 0 offs) (String.sub ns 0 len)
          (String.sub os (offs + len) (8 - offs - len)) in
      let helper env foo offset len s =
        find_reg64 env s
        >>= fun i ->
        return
          (MapString.add s
             (R64 (string_to_int64 (do_offset i (foo i) offset len)))
             env ) in
      match reg with
      | Reg r -> (
        match r with
        | s when reg8 s -> (
          match s with
          | s when reg8L s -> helper env foo 0 1 (Printf.sprintf "R%cX" s.[1])
          | s -> helper env foo 1 1 (Printf.sprintf "R%cX" s.[1]) )
        | s when reg16 s -> helper env foo 0 2 ("R" ^ s)
        | s when reg32 s -> helper env foo 0 4 ("R" ^ String.sub s 1 2)
        | s when reg64 s -> helper env foo 0 8 s
        | _ -> error "invalid combination of opcode and operands" )
      | _ -> error "invalid combination of opcode and operands" in
    let find_regsse env r =
      return (MapString.find r env)
      >>= function RSSE i -> return i | _ -> error "falal error" in
    let change_regsse env reg foo =
      match reg with
      | Reg r when regSSE r ->
          find_regsse env r >>= fun s -> return (MapString.add r (foo s) env)
      | _ -> error "invalid combination of opcode and operands" in
    let jump env st foo = function
      | Label l ->
          foo (MapString.find "0cond" env)
          >>= fun b ->
          if b then return (MapString.add "0jump" (Ls l) env, st)
          else return (env, st)
      | _ -> error "Illegal instruction (core dumped)" in
    let inter_arg0 env st = function
      | "RET" ->
          jump env (List.tl st)
            (function
              | R64 _ -> return true
              | _ -> error "fatal error: register 64 can only be R64" )
            (Label (List.hd st))
      | "SYSCALL" -> (
          find_reg64 env "RAX"
          >>= function
          | 1L (* system call - print *) -> (
              find_reg64 env "RDI"
              >>= function
              | 0L | 1L | 2L ->
                  find_reg64 env "RDI"
                  >>= fun mes ->
                  find_reg64 env "RDX"
                  >>= fun len ->
                  print_string
                    (String.sub (int64_to_string mes) 0
                       (min (String.length (int64_to_string mes)) (to_int len)) );
                  flush stdout;
                  return (env, st)
              | _ -> return (env, st) )
          | 60L (* system call - exit *) ->
              find_reg64 env "RDI"
              >>= fun code ->
              return (MapString.add "0retcode" (R64 (rem code 256L)) env, st)
          | i when i < 336L ->
              error
                (Printf.sprintf "syscall %s is not implemented" (to_string i))
          | _ -> return (env, st) )
      | _ -> error "fatal error: the interpreter could not parse this" in
    let inter_arg1 env st e = function
      | "INC" ->
          change_reg64 env e (fun i -> add i 1L) >>= fun env -> return (env, st)
      | "DEC" ->
          change_reg64 env e (fun i -> sub i 1L) >>= fun env -> return (env, st)
      | "NOT" ->
          change_reg64 env e (fun i -> neg i) >>= fun env -> return (env, st)
      | "NEG" ->
          change_reg64 env e (fun i -> add (neg i) 1L)
          >>= fun env -> return (env, st)
      | "PUSH" -> (
        match e with
        | Label l -> return (env, l :: st)
        | Reg r when reg64 r ->
            find_reg64 env r >>| fun i -> (env, int64_to_string i :: st)
        | _ -> error "byte data exceeds bounds" )
      | "POP" -> (
        match e with
        | Reg r ->
            return
              ( MapString.add r (R64 (string_to_int64 (List.hd st))) env
              , List.tl st )
        | _ -> error "byte data exceeds bounds" )
      | "CALL" -> (
        match e with
        | Label l ->
            jump env (l :: st)
              (function
                | R64 _ -> return true
                | _ -> error "fatal error: register 64 can only be R64" )
              e
        | _ -> error "Illegal instruction (core dumped)" )
      | "JMP" ->
          jump env st
            (function
              | R64 _ -> return true
              | _ -> error "fatal error: register 64 can only be R64" )
            e
      | "JE" ->
          jump env st
            (function
              | R64 0L -> return true
              | R64 _ -> return false
              | _ -> error "fatal error: register 64 can only be R64" )
            e
      | "JNE" ->
          jump env st
            (function
              | R64 0L -> return false
              | R64 _ -> return true
              | _ -> error "fatal error: register 64 can only be R64" )
            e
      | "JG" ->
          jump env st
            (function
              | R64 i when i > 0L -> return true
              | R64 _ -> return false
              | _ -> error "fatal error: register 64 can only be R64" )
            e
      | "JGE" ->
          jump env st
            (function
              | R64 i when i >= 0L -> return true
              | R64 _ -> return false
              | _ -> error "fatal error: register 64 can only be R64" )
            e
      | "JL" ->
          jump env st
            (function
              | R64 i when i < 0L -> return true
              | R64 _ -> return false
              | _ -> error "fatal error: register 64 can only be R64" )
            e
      | "JLE" ->
          jump env st
            (function
              | R64 i when i <= 0L -> return true
              | R64 _ -> return false
              | _ -> error "fatal error: register 64 can only be R64" )
            e
      | _ -> error "fatal error: the interpreter could not parse this" in
    let inter_arg2 env e1 e2 =
      let helper foo =
        ev env (f env) e2 >>= fun i -> change_reg64 env e1 (foo i) in
      let helper_sse foo =
        let foo1 s1 s2 =
          let news n s =
            if String.length s > n then String.sub s 0 n
            else s ^ String.make (n - String.length s) '\x00' in
          let half n =
            news 8
              (int64_to_string
                 (foo
                    (string_to_int64 (String.sub (news 16 s1) n 8))
                    (string_to_int64 (String.sub (news 16 s2) n 8)) ) ) in
          RSSE (half 0 ^ half 8) in
        ( match e2 with
        | Label l -> (
          match MapString.find l env with
          | Ls s -> return s
          | _ -> error "fatal error: label can only be Ls" )
        | Reg r when regSSE r -> (
          match MapString.find r env with
          | RSSE s -> return s
          | _ -> error "fatal error: SEE register can only be RSSE" )
        | _ -> error "invalid combination of opcode and operands" )
        >>= fun s -> change_regsse env e1 (foo1 s) in
      function
      | "MOV" -> helper (fun i _ -> i)
      | "ADD" -> helper (fun i s -> add s i)
      | "SUB" -> helper (fun i s -> sub s i)
      | "AND" -> helper (fun i s -> logand s i)
      | "OR" -> helper (fun i s -> logor s i)
      | "XOR" -> helper (fun i s -> logxor s i)
      | "IMUL" ->
          helper (fun i s ->
              mul (if s >= 0L then s else neg s) (if i >= 0L then i else neg i) )
      | "SHR" -> helper (fun i s -> shift_right_logical s (to_int i))
      | "SHL" -> helper (fun i s -> shift_left s (to_int i))
      | "CMP" ->
          ( match e1 with
          | Reg _ -> ev env (f env) (Sub (e1, e2))
          | _ -> error "invalid combination of opcode and operands" )
          >>= fun i -> return (MapString.add "0cond" (R64 i) env)
      | "ADDPD" -> helper_sse (fun i s -> add s i)
      | "SUBPD" -> helper_sse (fun i s -> sub s i)
      | "MULPD" -> helper_sse (fun i s -> mul s i)
      | "MOVAPD" -> (
        match e1 with
        | Reg r when regSSE r -> (
          match e2 with
          | Label s -> (
            match MapString.find s env with
            | Ls i | RSSE i -> return (MapString.add r (RSSE i) env)
            | _ -> error "fatal error: cannot be R64" )
          | Reg s when regSSE s -> (
            match MapString.find s env with
            | Ls i | RSSE i -> return (MapString.add r (RSSE i) env)
            | _ -> error "fatal error: cannot be R64" )
          | _ -> error "invalid combination of opcode and operands" )
        | _ -> error "invalid combination of opcode and operands" )
      | _ -> error "fatal error: the interpreter could not parse this" in
    let inter_cmd env st = function
      | _, Arg0 (Mnemonic mn) -> inter_arg0 env st mn
      | _, Arg1 (Mnemonic mn, e) -> inter_arg1 env st e mn
      | _, Arg2 (Mnemonic mn, e1, e2) ->
          inter_arg2 env e1 e2 mn >>= fun env -> return (env, st) in
    let rec helper env st =
      let rec find_label l = function
        | [] -> error (Printf.sprintf "symbol `%s\' not defined" l)
        | hd :: tl -> (
          match hd with
          | Some (Id id), _ -> (
            match id with
            | id when id = l -> return (hd :: tl)
            | _ -> find_label l tl )
          | _ -> find_label l tl ) in
      let ex_jmp env st foo =
        match MapString.find_opt "0retcode" env with
        | Some (R64 0L) -> return env
        | Some (R64 i) -> error ("Error " ^ to_string i)
        | _ -> (
          match MapString.find "0jump" env with
          | Ls "" -> foo ()
          | Ls s ->
              find_label s list
              >>= fun list -> helper (MapString.add "0jump" (Ls "") env) st list
          | _ -> error "fatal error: can be only Ls" ) in
      function
      | [] -> ex_jmp env st (fun () -> error "Segmentation fault (core dumped)")
      | hd :: tl ->
          inter_cmd env st hd
          >>= fun (env, st) -> ex_jmp env st (fun () -> helper env st tl) in
    helper env [] list

  (** Main interpreter function. *)
  let asm directive = prepr (MapString.of_list reg_list) directive >>= interpret
end

(** Helper functions for testing. *)

(* ************************* Tests ********************************* *)

open Eval (Result)

(* ------------------------ Helper ----------------------- *)
let empty_env = MapString.empty

(* -------------------- Preprocessing -------------------- *)

(* (var_t MapString.t * command list) M.t *)
let succ_prepr ?(env = empty_env) giv exp_env exp_res =
  match prepr env giv with
  | Ok res when (exp_env, exp_res) = res -> true
  | _ -> false

let fail_prepr ?(env = empty_env) giv exp_res =
  match prepr env giv with Error e when exp_res = e -> true | _ -> false

let%test _ =
  succ_prepr
    (Directive [Code [Instr (None, Arg2 (Mnemonic "MOV", Reg "RAX", Const 1L))]])
    empty_env
    [(None, Arg2 (Mnemonic "MOV", Reg "RAX", Const 1L))]

let%test _ =
  succ_prepr
    (Directive
       [ Code
           [ DataDecl
               ( Some (Id "None")
               , "DB"
               , [Expr (Add (Const 66L, Mul (Const 256L, Const 65L)))] ) ] ] )
    (MapString.singleton "None" (Ls "BA"))
    []

let%test _ =
  succ_prepr
    (Directive [Code [EqualDir (Id "l", Add (Const 1L, Const 1L))]])
    (MapString.singleton "l" (Ls "\x02"))
    []

let%test _ =
  fail_prepr
    (Directive [Code [EquDir (Id "l", Add (Label "fgf", Const 1L))]])
    "relocation truncated to fit: R_X86_64_8 against `.data'"

(* ------------------- Intrepretation -------------------- *)

let input_env = MapString.of_list reg_list

let succ_env_list =
  reg_list @ [("RAX", R64 60L); ("RDI", R64 0L); ("0retcode", R64 0L)]

let succ_env = MapString.of_list succ_env_list

let succ_simpl_dir =
  [ (None, Arg2 (Mnemonic "MOV", Reg "RDI", Const 0L))
  ; (None, Arg2 (Mnemonic "MOV", Reg "RAX", Const 60L))
  ; (None, Arg0 (Mnemonic "SYSCALL")) ]

(* ------------------------ Helper ----------------------- *)

(* (var_t MapString.t *)
let succ_inter env giv exp_env =
  match interpret (env, giv) with
  | Ok res when exp_env = res -> true
  | _ -> false

let fail_inter ?(env = MapString.of_list reg_list) giv exp_res =
  match interpret (env, giv) with
  | Error e when exp_res = e -> true
  | _ -> false

let%test _ = fail_inter [] "Segmentation fault (core dumped)"

let%test _ =
  succ_inter input_env
    [ (None, Arg2 (Mnemonic "MOV", Reg "RDI", Const 256L))
    ; (None, Arg2 (Mnemonic "MOV", Reg "RAX", Const 60L))
    ; (None, Arg0 (Mnemonic "SYSCALL")) ]
    (MapString.add "RDI" (R64 256L) succ_env)

let%test _ =
  succ_inter
    (MapString.add "message" (Ls "Hello!") input_env)
    ( [ (None, Arg2 (Mnemonic "MOV", Reg "RDX", Const 12L))
      ; (None, Arg2 (Mnemonic "MOV", Reg "RSI", Label "message"))
      ; (None, Arg2 (Mnemonic "MOV", Reg "RDI", Const 1L))
      ; (None, Arg2 (Mnemonic "MOV", Reg "RAX", Const 1L)) ]
    @ succ_simpl_dir )
    (MapString.of_list
       ( succ_env_list
       @ [ ("RDX", R64 12L); ("RSI", R64 36762444129608L)
         ; ("message", Ls "Hello!") ] ) )

let%test _ =
  succ_inter input_env
    ((None, Arg2 (Mnemonic "XOR", Reg "RBX", Reg "RBX")) :: succ_simpl_dir)
    succ_env

let%test _ =
  succ_inter input_env
    ( [ (Some (Id "j"), Arg1 (Mnemonic "INC", Reg "RBX"))
      ; (None, Arg2 (Mnemonic "CMP", Reg "RBX", Const 8L))
      ; (None, Arg1 (Mnemonic "JNE", Label "j")) ]
    @ succ_simpl_dir )
    (MapString.add "RBX" (R64 8L) succ_env)

let%test _ =
  succ_inter
    (MapString.of_list
       ( reg_list
       @ [ ("XMM2", RSSE "Oh hi")
         ; ("XMM3", RSSE "\x00\x00\x00\x00\x00, Mark!    ") ] ) )
    ((None, Arg2 (Mnemonic "ADDPD", Reg "XMM3", Reg "XMM2")) :: succ_simpl_dir)
    (MapString.of_list
       ( succ_env_list
       @ [("XMM2", RSSE "Oh hi"); ("XMM3", RSSE "Oh hi, Mark!    ")] ) )

let%test _ =
  succ_inter
    (MapString.of_list (reg_list @ [("XMM2", RSSE "b"); ("XMM3", RSSE "a")]))
    ((None, Arg2 (Mnemonic "SUBPD", Reg "XMM2", Reg "XMM3")) :: succ_simpl_dir)
    (MapString.of_list
       ( succ_env_list
       @ [ ( "XMM2"
           , RSSE
               "\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
           ); ("XMM3", RSSE "a") ] ) )
