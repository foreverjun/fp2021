open Ast
open Int64

module type MonadError = sig
  include Base.Monad.Infix

  val return : 'a -> 'a t
  val error : string -> 'a t
end

let is_arg0 = function "RET" | "SYSCALL" -> true | _ -> false

let is_arg1 = function
  | "PUSH" | "POP" | "INC" | "DEC" | "IDIV" | "NOT" | "NEG" | "JMP" | "JE"
   |"JNE" | "JZ" | "JG" | "JGE" | "JL" | "JLE" | "CALL" ->
      true
  | _ -> false

let is_arg2 = function
  | "MOV" | "LEA" | "ADD" | "SUB" | "IMUL" | "AND" | "OR" | "XOR" | "SHL"
   |"SHR" | "CMP" | "MOVUPS" | "MOVSS" | "MOVLPS" | "MOVHPS" | "ADDSS"
   |"MULSS" | "SUBSS" | "SHUFPS" ->
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
    let rec helper acc = function
      | "" -> acc
      | "\x80" -> neg acc
      | s ->
          helper
            (add (of_int (Char.code s.[0])) acc)
            (String.sub s 1 (String.length s - 1)) in
    helper 0L s

  let int64_to_string i =
    let rec helper acc = function
      | k when k = 0L -> acc
      | k when k = -1L -> acc ^ "\x80"
      | k ->
          let c = acc ^ String.make 1 (Char.chr (to_int (modulo k 256L))) in
          helper c (shift_right k 8) in
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
        else ev e f l >>= fun l -> return (shift_right l (to_int r mod 64))
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
      | Some _ -> error ("label `" ^ id ^ "\' inconsistently redefined") in
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
    ; ("XMM6", RSSE "\x00"); ("XMM7", RSSE "\x00") ]

  let interpret (env, list) =
    let f e = function
      | Reg _ -> error "expression is not simple or relocatable"
      | Label l -> (
        match MapString.find_opt l e with
        | Some (Ls s) -> return (string_to_int64 s)
        | Some Er -> error "byte data exceeds bounds"
        | _ -> error "relocation truncated to fit: R_X86_64_8 against `.data\'"
        )
      | _ -> error "fatal error" in
    let change_reg env3 e foo =
      match e with
      | Reg r -> return (MapString.add r (foo (MapString.find r env3)) env3)
      | _ -> error "invalid combination of opcode and operands" in
    let inter_arg0 env2 st = function
      | "RET" -> error "TODO"
      | "SYSCALL" -> error "TODO"
      | _ -> error "fatal error" in
    let inter_arg1 env2 st e = function
      | "INC" ->
          change_reg env2 e (fun i -> add i 1L) >>= fun env3 -> return (env3, st)
      | "DEC" ->
          change_reg env2 e (fun i -> sub i 1L) >>= fun env3 -> return (env3, st)
      | "NOT" ->
          change_reg env2 e (fun i -> neg i) >>= fun env3 -> return (env3, st)
      | "NEG" ->
          change_reg env2 e (fun i -> add (neg i) 1L)
          >>= fun env3 -> return (env3, st)
      | "PUSH" | "POP" | "IDIV" | "JMP" | "JE" | "JNE" | "JZ" | "JG" | "JGE"
       |"JL" | "JLE" | "CALL" ->
          error "TODO"
      | _ -> error "fatal error" in
    let inter_arg2 env2 e1 e2 = function
      | "MOV" | "LEA" | "INC" | "DEC" | "IDIV" | "ADD" | "SUB" | "IMUL"
       |"AND" | "OR" | "XOR" | "SHL" | "SHR" | "CMP" | "MOVUPS" | "MOVSS"
       |"MOVLPS" | "MOVHPS" | "ADDSS" | "MULSS" | "SUBSS" | "SHUFPS" ->
          error "TODO"
      | _ -> error "fatal error" in
    let inter_cmd env1 st = function
      | Arg0 (_, Mnemonic mn) -> inter_arg0 env1 st mn (* ret (env2, st) *)
      | Arg1 (_, Mnemonic mn, e) -> inter_arg1 env1 st e mn (* ret (env2, st) *)
      | Arg2 (_, Mnemonic mn, e1, e2) ->
          inter_arg2 env1 e1 e2 mn >>= fun env2 -> return (env2, st) in
    let rec helper env0 st = function
      | [] -> return ()
      | hd :: tl ->
          inter_cmd env0 st hd >>= fun (env2, st1) -> helper env2 st1 tl in
    return Stack.create >>= fun st -> helper env st list

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
