open Ast
open Int64

module type MonadError = sig
  include Base.Monad.Infix

  val return : 'a -> 'a t
  val error : string -> 'a t
end

let is_arg0 = function "RET" | "SYSCALL" -> true | _ -> false

let is_arg1 = function
  | "PUSH" | "POP" | "INC" | "DEC" | "NOT" | "NEG" | "JMP" | "JE" | "JNE"
   |"JG" | "JGE" | "JL" | "JLE" | "CALL" ->
      true
  | _ -> false

let is_arg2 = function
  | "MOV" | "ADD" | "SUB" | "IMUL" | "AND" | "OR" | "XOR" | "SHL" | "SHR"
   |"CMP" | "ADDPD" | "SUBPD" | "MULPD" | "MOVAPD" ->
      true
  | _ -> false

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

module Eval (M : MonadError) = struct
  open M

  (** Container for variables. *)
  type var_t =
    (* registers *)
    | R64 of int64
    | RSSE of string (* 128 bit *)
    (* constants *)
    | Ls of string
    (* labels whose call will cause an error *)
    | Er
  [@@deriving show {with_path= false}]

  (** Enviroment variables. *)
  type env = var_t MapString.t [@@deriving show {with_path= false}]

  let modulo x y =
    let result = rem x y in
    if result >= 0L then result else add result y

  let string_to_int64 s =
    let rec helper acc n = function
      | "" -> acc
      | s when String.length s = 1 ->
          let c = of_int (Char.code s.[0]) in
          let res = add acc (shift_left (logand 0x7FL c) n) in
          if logand 0x80L c = 0L then res else neg res
      | s ->
          helper
            (add (shift_left (of_int (Char.code s.[0])) n) acc)
            (n + 8)
            (String.sub s 1 (String.length s - 1)) in
    helper 0L 0 s

  let int64_to_string i =
    let rec helper acc = function
      | k when k = 0L -> acc
      | k when k = -1L -> acc ^ "\x80"
      | k ->
          let c = acc ^ String.make 1 (Char.chr (to_int (modulo k 256L))) in
          helper c (shift_right_logical k 8) in
    helper "" i

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

  let prepr env =
    let pr_expr env17 =
      let f e = function
        | Reg _ -> error "expression is not simple or relocatable"
        | Label l -> (
          match MapString.find_opt l e with
          | Some (Ls s) -> return (string_to_int64 s)
          | Some Er -> error "byte data exceeds bounds"
          | _ ->
              error "relocation truncated to fit: R_X86_64_8 against `.data\'" )
        | _ -> error "fatal error" in
      function
      | Label l -> (
        match MapString.find_opt l env17 with
        | Some (Ls s) -> return (Ls s)
        | Some Er -> error "byte data exceeds bounds"
        | _ -> error "relocation truncated to fit: R_X86_64_8 against `.data\'"
        )
      | Reg _ -> error "expression is not simple or relocatable"
      | e -> ev env17 (f env17) e >>= fun i -> return (Ls (int64_to_string i))
    in
    let str_to_lb s = return (Ls s) in
    let env_add id v env9 =
      match MapString.find_opt id env9 with
      | None -> return (MapString.add id v env9)
      | Some _ ->
          error
            (String.concat "" ["label `"; id; "\' inconsistently redefined"])
    in
    let env_set id v env9 =
      match MapString.find_opt id env9 with
      | None -> return (MapString.add id v env9)
      | Some _ -> return (MapString.add id v env9) in
    let rec pr_init_val env16 acc =
      let helper env17 = function
        | Str s -> return (Ls (acc ^ s))
        | Expr e -> pr_expr env17 e
        | Dup (e, s) -> (
            pr_expr env17 e
            >>=
            let rec helper s acc d =
              if d = 0L then return acc else helper s (acc ^ s) (sub d 1L) in
            function
            | Ls s1 -> helper s "" (string_to_int64 s1) >>| fun s1 -> Ls s1
            | _ -> error "fatal error" ) in
      function
      | [] ->
          if acc = "" then error "no operand for data declaration"
          else return acc
      | hd :: tl -> (
          helper env16 hd
          >>= function
          | Ls s -> pr_init_val env16 (acc ^ s) tl | _ -> error "fatal error" )
    in
    let pr_equdir env6 = function
      | EquDir (Id id, e) -> pr_expr env6 e >>= fun v -> env_add id v env6
      | EqualDir (Id id, e) -> pr_expr env6 e >>= fun v -> env_set id v env6
      | _ -> error "fatal error" in
    let pr_dd env11 id =
      let helper env12 list = function
        | Some (Id i) ->
            pr_init_val env12 "" list >>= str_to_lb
            >>= fun v -> env_add i v env12
        | None -> return env12 in
      fun (_, iv) -> helper env11 iv id in
    let rec pr_data env7 =
      let pr_inst env8 id =
        let helper env9 = function
          | Some (Id i) -> env_add i Er env9
          | None -> return env9 in
        function
        | Instruction (Mnemonic s, []) when is_arg0 s -> helper env8 id
        | Instruction (Mnemonic s, [_]) when is_arg1 s -> helper env8 id
        | Instruction (Mnemonic s, [_; _]) when is_arg2 s -> helper env8 id
        | Instruction _ -> error "invalid combination of opcode and operands"
        | DataDecl (dd, l) -> pr_dd env8 id (dd, l) in
      let helper env5 = function
        | InDir (id, isd) -> pr_inst env5 id isd
        | d -> pr_equdir env5 d in
      function
      | [] -> return env7
      | hd :: tl -> helper env7 hd >>= fun env8 -> pr_data env8 tl in
    let rec pr_code env4 acc4 =
      let pr_inst env12 id = function
        | Instruction (Mnemonic s, []) when is_arg0 s ->
            return (env12, [Arg0 (id, Mnemonic s)])
        | Instruction (Mnemonic s, [a1]) when is_arg1 s ->
            return (env12, [Arg1 (id, Mnemonic s, a1)])
        | Instruction (Mnemonic s, [a1; a2]) when is_arg2 s ->
            return (env12, [Arg2 (id, Mnemonic s, a1, a2)])
        | Instruction _ -> error "invalid combination of opcode and operands"
        | DataDecl (dd, l) -> pr_dd env12 id (dd, l) >>| fun env13 -> (env13, [])
      in
      let helper env5 = function
        | InDir (id, isd) -> pr_inst env5 id isd
        | d -> pr_equdir env5 d >>| fun env6 -> (env6, []) in
      function
      | [] -> return (env4, acc4)
      | hd :: tl ->
          helper env4 hd >>= fun (env1, l2) -> pr_code env1 (acc4 @ l2) tl in
    let rec pr_sec_dir env1 acc1 =
      let helper env2 = function
        | Section ("DATA", l1) | Section ("CONST", l1) ->
            pr_data env2 l1 >>| fun env3 -> (env3, [])
        | Section ("TEXT", l1) | Section ("CODE", l1) ->
            pr_code env2 [] l1 >>| fun (env3, acc2) -> (env3, acc2)
        | _ -> error "fatal error" in
      function
      | [] -> return (env1, acc1)
      | hd :: tl ->
          helper env1 hd
          >>= fun (env2, acc2) -> (pr_sec_dir [@tailcall]) env2 (acc1 @ acc2) tl
    in
    fun (Directive list) -> pr_sec_dir env [] list

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

  let interpret (env, list) =
    let f e = function
      | Reg r -> (
        match MapString.find r e with
        | R64 i -> return i
        | RSSE _ -> error "invalid combination of opcode and operands"
        | _ -> error "fatal error" )
      | Label l -> (
        match MapString.find_opt l e with
        | Some (Ls s) -> return (string_to_int64 s)
        | Some Er -> error "byte data exceeds bounds"
        | _ -> error (String.concat "" ["symbol `"; l; "\' not defined"]) )
      | _ -> error "fatal error" in
    let find_reg64 env3 r =
      return (MapString.find r env3)
      >>= function R64 i -> return i | _ -> error "falal error" in
    let change_reg64 env3 reg foo =
      let do_offset ot nt offs len =
        let os =
          let h = int64_to_string ot in
          h ^ String.make (8 - String.length h) '\x00' in
        let ns =
          let h = int64_to_string nt in
          h ^ String.make (8 - String.length h) '\x00' in
        String.concat ""
          [ String.sub os 0 offs; String.sub ns 0 len
          ; String.sub os (offs + len) (8 - offs - len) ] in
      let helper env4 foo offset len s =
        find_reg64 env3 s
        >>= fun i ->
        return
          (MapString.add s
             (R64 (string_to_int64 (do_offset i (foo i) offset len)))
             env4 ) in
      match reg with
      | Reg r -> (
        match r with
        | s when reg8 s -> (
          match s with
          | s when reg8L s ->
              helper env3 foo 0 1
                (String.concat "" ["R"; String.sub s 1 1; "X"])
          | s ->
              helper env3 foo 1 1
                (String.concat "" ["R"; String.sub s 1 1; "X"]) )
        | s when reg16 s -> helper env3 foo 0 2 ("R" ^ s)
        | s when reg32 s -> helper env3 foo 0 4 ("R" ^ String.sub s 1 2)
        | s when reg64 s -> helper env3 foo 0 8 s
        | _ -> error "invalid combination of opcode and operands" )
      | _ -> error "invalid combination of opcode and operands" in
    let find_regsse env3 r =
      return (MapString.find r env3)
      >>= function RSSE i -> return i | _ -> error "falal error" in
    let change_regsse env3 reg foo =
      match reg with
      | Reg r when regSSE r ->
          find_regsse env3 r >>= fun s -> return (MapString.add r (foo s) env3)
      | _ -> error "invalid combination of opcode and operands" in
    let jump env2 st foo = function
      | Label l ->
          foo (MapString.find "0cond" env2)
          >>= fun b ->
          if b then return (MapString.add "0jump" (Ls l) env2, st)
          else return (env2, st)
      | _ -> error "Illegal instruction (core dumped)" in
    let inter_arg0 env2 st = function
      | "RET" ->
          jump env2 (List.tl st)
            (function R64 _ -> return true | _ -> error "fatal error")
            (Label (List.hd st))
      | "SYSCALL" -> (
          find_reg64 env2 "RAX"
          >>= function
          | 1L -> (
              find_reg64 env2 "RDI"
              >>= function
              | 0L | 1L | 2L ->
                  find_reg64 env2 "RDI"
                  >>= fun mes ->
                  find_reg64 env2 "RDX"
                  >>= fun len ->
                  print_string
                    (String.sub (int64_to_string mes) 0
                       (min (String.length (int64_to_string mes)) (to_int len)) );
                  flush stdout;
                  return (env2, st)
              | _ -> return (env2, st) )
          | 60L ->
              find_reg64 env2 "RDI"
              >>= fun code ->
              return (MapString.add "0retcode" (R64 (rem code 256L)) env2, st)
          | i when i < 336L ->
              error
                (String.concat ""
                   ["syscall "; to_string i; " is not implemented"] )
          | _ -> return (env2, st) )
      | _ -> error "fatal error" in
    let inter_arg1 env2 st e = function
      | "INC" ->
          change_reg64 env2 e (fun i -> add i 1L)
          >>= fun env3 -> return (env3, st)
      | "DEC" ->
          change_reg64 env2 e (fun i -> sub i 1L)
          >>= fun env3 -> return (env3, st)
      | "NOT" ->
          change_reg64 env2 e (fun i -> neg i) >>= fun env3 -> return (env3, st)
      | "NEG" ->
          change_reg64 env2 e (fun i -> add (neg i) 1L)
          >>= fun env3 -> return (env3, st)
      | "PUSH" -> (
        match e with
        | Label l -> return (env2, [l] @ st)
        | _ -> error "byte data exceeds bounds" )
      | "POP" -> (
        match e with
        | Reg r ->
            return
              ( MapString.add r (R64 (string_to_int64 (List.hd st))) env2
              , List.tl st )
        | _ -> error "byte data exceeds bounds" )
      | "CALL" -> (
        match e with
        | Label l ->
            jump env2
              ([l] @ st)
              (function R64 _ -> return true | _ -> error "fatal error")
              e
        | _ -> error "Illegal instruction (core dumped)" )
      | "JMP" ->
          jump env2 st
            (function R64 _ -> return true | _ -> error "fatal error")
            e
      | "JE" ->
          jump env2 st
            (function
              | R64 0L -> return true
              | R64 _ -> return false
              | _ -> error "fatal error" )
            e
      | "JNE" ->
          jump env2 st
            (function
              | R64 0L -> return false
              | R64 _ -> return true
              | _ -> error "fatal error" )
            e
      | "JG" ->
          jump env2 st
            (function
              | R64 i when i > 0L -> return true
              | R64 _ -> return false
              | _ -> error "fatal error" )
            e
      | "JGE" ->
          jump env2 st
            (function
              | R64 i when i >= 0L -> return true
              | R64 _ -> return false
              | _ -> error "fatal error" )
            e
      | "JL" ->
          jump env2 st
            (function
              | R64 i when i < 0L -> return true
              | R64 _ -> return false
              | _ -> error "fatal error" )
            e
      | "JLE" ->
          jump env2 st
            (function
              | R64 i when i <= 0L -> return true
              | R64 _ -> return false
              | _ -> error "fatal error" )
            e
      | _ -> error "fatal error" in
    let inter_arg2 env2 e1 e2 =
      let helper foo =
        ev env2 (f env2) e2 >>= fun i -> change_reg64 env2 e1 (foo i) in
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
          match MapString.find l env2 with
          | Ls s -> return s
          | _ -> error "fatal error" )
        | Reg r when regSSE r -> (
          match MapString.find r env2 with
          | RSSE s -> return s
          | _ -> error "fatal error" )
        | _ -> error "invalid combination of opcode and operands" )
        >>= fun s -> change_regsse env2 e1 (foo1 s) in
      function
      | "MOV" -> helper (fun i _ -> i)
      | "ADD" -> helper (fun i s -> add s i)
      | "SUB" -> helper (fun i s -> sub s i)
      | "AND" -> helper (fun i s -> logand s i)
      | "OR" -> helper (fun i s -> logand s i)
      | "XOR" -> helper (fun i s -> logxor s i)
      | "IMUL" ->
          helper (fun i s ->
              mul (if s >= 0L then s else neg s) (if i >= 0L then i else neg i) )
      | "SHR" -> helper (fun i s -> shift_right_logical s (to_int i))
      | "SHL" -> helper (fun i s -> shift_left s (to_int i))
      | "CMP" ->
          ( match e1 with
          | Reg _ -> ev env2 (f env2) (Sub (e1, e2))
          | _ -> error "invalid combination of opcode and operands" )
          >>= fun i -> return (MapString.add "0cond" (R64 i) env2)
      | "ADDPD" -> helper_sse (fun i s -> add s i)
      | "SUBPD" -> helper_sse (fun i s -> sub s i)
      | "MULPD" -> helper_sse (fun i s -> mul s i)
      | "MOVAPD" -> (
        match e1 with
        | Reg r when regSSE r -> (
          match e2 with
          | Label s -> (
            match MapString.find s env2 with
            | Ls i | RSSE i -> return (MapString.add r (RSSE i) env2)
            | _ -> error "fatal error" )
          | Reg s when regSSE s -> (
            match MapString.find s env2 with
            | Ls i | RSSE i -> return (MapString.add r (RSSE i) env2)
            | _ -> error "fatal error" )
          | _ -> error "invalid combination of opcode and operands" )
        | _ -> error "invalid combination of opcode and operands" )
      | _ -> error "fatal error" in
    let inter_cmd env1 st = function
      | Arg0 (_, Mnemonic mn) -> inter_arg0 env1 st mn
      | Arg1 (_, Mnemonic mn, e) -> inter_arg1 env1 st e mn
      | Arg2 (_, Mnemonic mn, e1, e2) ->
          inter_arg2 env1 e1 e2 mn >>= fun env2 -> return (env2, st) in
    let rec helper env0 st =
      let rec find_label l = function
        | [] -> error (String.concat "" ["symbol `"; l; "\' not defined"])
        | hd :: tl -> (
          match hd with
          | Arg0 (Some (Id id), _)
           |Arg1 (Some (Id id), _, _)
           |Arg2 (Some (Id id), _, _, _) ->
              if id = l then return (hd :: tl) else find_label l tl
          | _ -> find_label l tl ) in
      function
      | [] -> (
        match MapString.find_opt "0retcode" env0 with
        | Some (R64 0L) -> return env0
        | Some (R64 i) -> error ("Error " ^ to_string i)
        | _ -> (
          match MapString.find "0jump" env0 with
          | Ls "" -> error "Segmentation fault (core dumped)"
          | Ls s ->
              find_label s list
              >>= fun list2 ->
              helper (MapString.add "0jump" (Ls "") env0) st list2
          | _ -> error "fatal error" ) )
      | hd :: tl -> (
          inter_cmd env0 st hd
          >>= fun (env2, st1) ->
          match MapString.find_opt "0retcode" env2 with
          | Some (R64 0L) -> return env2
          | Some (R64 i) -> error ("Error " ^ to_string i)
          | _ -> (
            match MapString.find "0jump" env2 with
            | Ls "" -> helper env2 st1 tl
            | Ls s ->
                find_label s list
                >>= fun list2 ->
                helper (MapString.add "0jump" (Ls "") env2) st1 list2
            | _ -> error "fatal error" ) ) in
    helper env [] list

  let asm directive = prepr (MapString.of_list reg_list) directive >>= interpret
end

(** / **)

(* ************************* Tests ********************************* *)

open Eval (Result)

(* ------------------------ Helper ----------------------- *)
let empty_env = MapString.empty

(* -------------------- Preprocessing -------------------- *)

(* (var_t MapString.t * command list) M.t *)
let succ_prepr ?(env = empty_env) giv exp_env exp_res =
  match prepr env giv with
  | Error e ->
      Printf.printf "Error: %s\n" e;
      false
  | Ok res when (exp_env, exp_res) = res -> true
  | Ok (env1, list) ->
      print_string "\n-------------------- Input --------------------\n";
      pp_directive Format.std_formatter giv;
      Format.pp_print_flush Format.std_formatter ();
      print_string "\n------------- Actual Environment --------------\n";
      pp_env Format.std_formatter env1;
      Format.pp_print_flush Format.std_formatter ();
      print_string "\n------------- Expected Environment ------------\n";
      pp_env Format.std_formatter exp_env;
      Format.pp_print_flush Format.std_formatter ();
      print_string "\n----------------- Actual Result ---------------\n";
      pp_simpl_dir Format.std_formatter list;
      Format.pp_print_flush Format.std_formatter ();
      print_string "\n---------------- Expected Result --------------\n";
      pp_simpl_dir Format.std_formatter exp_res;
      Format.pp_print_flush Format.std_formatter ();
      print_string "\n-----------------------------------------------\n";
      flush stdout;
      false

let fail_prepr ?(env = empty_env) giv exp_res =
  match prepr env giv with
  | Error e when exp_res = e -> true
  | Error e ->
      print_string "\n-------------------- Input --------------------\n";
      pp_directive Format.std_formatter giv;
      Format.pp_print_flush Format.std_formatter ();
      print_string "\n--------------- Unexpected error --------------\n";
      print_string e;
      print_string "\n-----------------------------------------------\n";
      flush stdout;
      false
  | Ok (env1, list) ->
      print_string "\n-------------------- Input --------------------\n";
      pp_directive Format.std_formatter giv;
      Format.pp_print_flush Format.std_formatter ();
      print_string "\n------------- Actual Environment --------------\n";
      pp_env Format.std_formatter env1;
      Format.pp_print_flush Format.std_formatter ();
      print_string "\n----------------- Actual Result ---------------\n";
      pp_simpl_dir Format.std_formatter list;
      Format.pp_print_flush Format.std_formatter ();
      flush stdout;
      false

let%test _ =
  succ_prepr
    (Directive
       [ Section
           ( "TEXT"
           , [InDir (None, Instruction (Mnemonic "MOV", [Reg "RAX"; Const 1L]))]
           ) ] )
    empty_env
    [Arg2 (None, Mnemonic "MOV", Reg "RAX", Const 1L)]

let%test _ =
  succ_prepr
    (Directive
       [ Section
           ( "TEXT"
           , [ InDir
                 ( Some (Id "None")
                 , DataDecl
                     ( "DB"
                     , [Expr (Add (Const 66L, Mul (Const 256L, Const 65L)))] )
                 ) ] ) ] )
    (MapString.singleton "None" (Ls "BA"))
    []

let%test _ =
  succ_prepr
    (Directive [Section ("TEXT", [EqualDir (Id "l", Add (Const 1L, Const 1L))])])
    (MapString.singleton "l" (Ls "\x02"))
    []

let%test _ =
  fail_prepr
    (Directive [Section ("TEXT", [EquDir (Id "l", Add (Label "fgf", Const 1L))])]
    )
    "relocation truncated to fit: R_X86_64_8 against `.data'"

(* ------------------- Intrepretation -------------------- *)

let input_env = MapString.of_list reg_list

let succ_env_list =
  reg_list @ [("RAX", R64 60L); ("RDI", R64 0L); ("0retcode", R64 0L)]

let succ_env = MapString.of_list succ_env_list

let succ_simpl_dir =
  [ Arg2 (None, Mnemonic "MOV", Reg "RDI", Const 0L)
  ; Arg2 (None, Mnemonic "MOV", Reg "RAX", Const 60L)
  ; Arg0 (None, Mnemonic "SYSCALL") ]

(* ------------------------ Helper ----------------------- *)

(* (var_t MapString.t *)
let succ_inter env giv exp_env =
  match interpret (env, giv) with
  | Error e ->
      Printf.printf "Error: %s\n" e;
      false
  | Ok res when exp_env = res -> true
  | Ok res ->
      print_string "\n-------------------- Input --------------------\n";
      pp_simpl_dir Format.std_formatter giv;
      Format.pp_print_flush Format.std_formatter ();
      print_string "\n------------- Actual Environment --------------\n";
      pp_env Format.std_formatter res;
      Format.pp_print_flush Format.std_formatter ();
      print_string "\n------------- Expected Environment ------------\n";
      pp_env Format.std_formatter exp_env;
      Format.pp_print_flush Format.std_formatter ();
      print_string "\n-----------------------------------------------\n";
      flush stdout;
      false

let fail_inter ?(env = MapString.of_list reg_list) giv exp_res =
  match interpret (env, giv) with
  | Error e when exp_res = e -> true
  | Error e ->
      print_string "\n-------------------- Input --------------------\n";
      pp_simpl_dir Format.std_formatter giv;
      Format.pp_print_flush Format.std_formatter ();
      print_string "\n--------------- Unexpected error --------------\n";
      print_string e;
      print_string "\n-----------------------------------------------\n";
      flush stdout;
      false
  | Ok env1 ->
      print_string "\n-------------------- Input --------------------\n";
      pp_simpl_dir Format.std_formatter giv;
      Format.pp_print_flush Format.std_formatter ();
      print_string "\n------------- Actual Environment --------------\n";
      pp_env Format.std_formatter env1;
      Format.pp_print_flush Format.std_formatter ();
      print_string "\n-----------------------------------------------\n";
      flush stdout;
      false

let%test _ = fail_inter [] "Segmentation fault (core dumped)"

let%test _ =
  succ_inter input_env
    [ Arg2 (None, Mnemonic "MOV", Reg "RDI", Const 256L)
    ; Arg2 (None, Mnemonic "MOV", Reg "RAX", Const 60L)
    ; Arg0 (None, Mnemonic "SYSCALL") ]
    (MapString.add "RDI" (R64 256L) succ_env)

let%test _ =
  succ_inter
    (MapString.add "message" (Ls "Hello!") input_env)
    ( [ Arg2 (None, Mnemonic "MOV", Reg "RDX", Const 12L)
      ; Arg2 (None, Mnemonic "MOV", Reg "RSI", Label "message")
      ; Arg2 (None, Mnemonic "MOV", Reg "RDI", Const 1L)
      ; Arg2 (None, Mnemonic "MOV", Reg "RAX", Const 1L) ]
    @ succ_simpl_dir )
    (MapString.of_list
       ( succ_env_list
       @ [ ("RDX", R64 12L); ("RSI", R64 36762444129608L)
         ; ("message", Ls "Hello!") ] ) )

let%test _ =
  succ_inter input_env
    ([Arg2 (None, Mnemonic "XOR", Reg "RBX", Reg "RBX")] @ succ_simpl_dir)
    succ_env

let%test _ =
  succ_inter input_env
    ( [ Arg1 (Some (Id "j"), Mnemonic "INC", Reg "RBX")
      ; Arg2 (None, Mnemonic "CMP", Reg "RBX", Const 8L)
      ; Arg1 (None, Mnemonic "JNE", Label "j") ]
    @ succ_simpl_dir )
    (MapString.add "RBX" (R64 8L) succ_env)

let%test _ =
  succ_inter
    (MapString.of_list
       ( reg_list
       @ [ ("XMM2", RSSE "Oh hi")
         ; ("XMM3", RSSE "\x00\x00\x00\x00\x00, Mark!    ") ] ) )
    ([Arg2 (None, Mnemonic "ADDPD", Reg "XMM3", Reg "XMM2")] @ succ_simpl_dir)
    (MapString.of_list
       ( succ_env_list
       @ [("XMM2", RSSE "Oh hi"); ("XMM3", RSSE "Oh hi, Mark!    ")] ) )

let%test _ =
  succ_inter
    (MapString.of_list (reg_list @ [("XMM2", RSSE "b"); ("XMM3", RSSE "a")]))
    ([Arg2 (None, Mnemonic "SUBPD", Reg "XMM2", Reg "XMM3")] @ succ_simpl_dir)
    (MapString.of_list
       ( succ_env_list
       @ [ ( "XMM2"
           , RSSE
               "\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
           ); ("XMM3", RSSE "a") ] ) )
