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
    | RB of string
    (* constants *)
    | Ls of string
    | Lb of string
    (* labels whose call will cause an error *)
    | Er
  [@@deriving show {with_path= false}]

  (** Enviroment variables. *)
  type env = var_t MapString.t [@@deriving show {with_path= false}]

  (* let ev_expr env =
       let find_reg s =
         match MapString.find_opt s env with
         | Some (R v) -> return v
         | _ -> error ("symbol `" ^ s ^ "\' not defined") in
       let find_label s =
         let bytes_to_int64 bs =
           let helper = Bytes.sub bs 0 (min (Bytes.length bs) 64) in
           let rec calc b i =
             if Bytes.length b > 0 then
               calc
                 (Bytes.sub b 1 (Bytes.length b - 1))
                 (add (shift_right i 8) (of_int (Char.code (Bytes.get b 0))))
             else i in
           calc helper 0L in
         match MapString.find_opt s env with
         | Some (L v) -> return (bytes_to_int64 v)
         | _ -> error ("symbol `" ^ s ^ "\' not defined") in
       let rec ev =
         let modulo x y =
           let result = sub x (mul (div x y) y) in
           if result >= 0L then result else add result y in
         function
         | Const n -> return n
         | Add (l, r) -> ev l >>= fun l -> ev r >>= fun r -> return (add l r)
         | Sub (l, r) -> ev l >>= fun l -> ev r >>= fun r -> return (sub l r)
         | Mul (l, r) -> ev l >>= fun l -> ev r >>= fun r -> return (mul l r)
         | Div (l, r) ->
             ev r
             >>= fun r ->
             if r = 0L then error "division by zero"
             else ev l >>= fun l -> return (div l r)
         | Mod (l, r) ->
             ev r
             >>= fun r ->
             if r = 0L then error "division by zero"
             else ev l >>= fun l -> return (modulo l r)
         | Shl (l, r) ->
             ev r
             >>= fun r ->
             if r < 0L then error "shift by a negative value"
             else ev l >>= fun l -> return (shift_left l (to_int r mod 64))
         | Shr (l, r) ->
             ev r
             >>= fun r ->
             if r < 0L then error "shift by a negative value"
             else ev l >>= fun l -> return (shift_right l (to_int r mod 64))
         | Reg s -> find_reg s
         | Label s -> find_label s in
       ev

     let prepr1 env =
       let pr_expr env =
         let modulo x y =
           let result = sub x (mul (div x y) y) in
           if result >= 0L then result else add result y in
         let int64_to_bytes i =
           let rec helper acc k n =
             if k != 0L then (
               Bytes.set acc n (Char.chr (to_int (modulo k 256L)));
               helper acc (shift_right k 8) (n + 1) )
             else acc in
           helper (Bytes.make 8 '\x00') i 0 in
         let find_label s =
           match MapString.find_opt s env with
           | Some (L v) -> return (of_int (Char.code (Bytes.get v 0)))
           | _ -> error "relocation truncated to fit: R_X86_64_8 against `.data\'"
         in
         let rec ev = function
           | Const n -> return n
           | Add (l, r) -> ev l >>= fun l -> ev r >>= fun r -> return (add l r)
           | Sub (l, r) -> ev l >>= fun l -> ev r >>= fun r -> return (sub l r)
           | Mul (l, r) -> ev l >>= fun l -> ev r >>= fun r -> return (mul l r)
           | Div (l, r) ->
               ev r
               >>= fun r ->
               if r = 0L then error "division by zero"
               else ev l >>= fun l -> return (div l r)
           | Mod (l, r) ->
               ev r
               >>= fun r ->
               if r = 0L then error "division by zero"
               else ev l >>= fun l -> return (modulo l r)
           | Shl (l, r) ->
               ev r
               >>= fun r ->
               if r < 0L then error "shift by a negative value"
               else ev l >>= fun l -> return (shift_left l (to_int r mod 64))
           | Shr (l, r) ->
               ev r
               >>= fun r ->
               if r < 0L then error "shift by a negative value"
               else ev l >>= fun l -> return (shift_right l (to_int r mod 64))
           | Reg _ -> error "expression is not simple or relocatable"
           | Label s -> find_label s in
         ev in
       let rec pr_isd =
         let helper = function
           | EqualDir (id, e) -> MapString.add_seq evn ()
           | EquDir (id, e) -> ()
           | InDir (Some id, inseg) -> ()
           | _ -> () in
         function hd :: tl -> helper hd; pr_isd tl | [] -> () in
       let rec pr_sec_dir acc env =
         let helper = function
           | Section ("DATA", l) -> pr_isd l; []
           | Section ("CONST", l) -> pr_isd l; []
           | sec -> [sec] in
         function
         | hd :: tl -> pr_sec_dir (acc @ helper hd) tl | [] -> (Directive acc, env)
       in
       fun (Directive list) -> pr_sec_dir [] list env

     (* let ev_init_value env = function
        | Expr e -> ev_expr env e
        | Str s -> return (Bytes.of_string s) *)

     let prepr2 env =

       let rec pr_expr env8 =
         let set_label s v =
           match MapString.find_opt s env with
           | Some (L _) -> error ("label `" ^ s ^ "\' inconsistently redefined")
           | _ -> return (MapString.add s v env8)
         in
         function
       | Reg _ -> error "expression is not simple or relocatable"
       | Label s -> (match MapString.find_opt s env8 with
           | Some (L v) -> return (L v)
           | _ -> error "relocation truncated to fit: R_X86_64_8 against `.data\'")
       | _ -> error ""
       in
       let rec pr_data env5 =
         let helper env7 = function
           | EqualDir (Id id, e)
           | EquDir (Id id, e) ->  pr_expr env7 id e >>= (function
             |  )
           | InDir (Some id, inseg) -> env7 (* TODO *)
           | _ -> env7 in
         function
         | hd :: tl -> ( match helper env5 hd with env6 -> pr_data env6 tl )
         | [] -> (env5, []) in
       let rec pr_sec_dir env1 acc1 =
         let helper env3 = function
           | Section ("DATA", l1) | Section ("CONST", l1) -> (
             match pr_data env3 l1 with env4, _ -> (env4, acc1) )
           | sec -> (env1, acc1 @ [sec])
           (* TODO *) in
         function
         | hd :: tl -> (
           match helper env1 hd with env2, l -> pr_sec_dir env2 l tl )
         | [] -> (env1, acc1) in
       fun (Directive list) -> pr_sec_dir env [] list *)

  let prepr env =
    let modulo x y =
      let result = rem x y in
      if result >= 0L then result else add result y in
    let string_to_int64 s =
      let rec helper acc = function
        | "" -> acc
        | "\x80" -> neg acc
        | s ->
            helper
              (add (of_int (Char.code s.[0])) acc)
              (String.sub s 1 (String.length s - 1)) in
      helper 0L s in
    let int64_to_string i =
      let rec helper acc = function
        | k when k = 0L -> acc
        | k when k = -1L -> acc ^ "\x80"
        | k ->
            let c = acc ^ String.make 1 (Char.chr (to_int (modulo k 256L))) in
            helper c (shift_right k 8) in
      helper "" i in
    let pr_expr env17 =
      let rec ev e = function
        | Const n -> return n
        | Add (l, r) -> ev e l >>= fun l -> ev e r >>= fun r -> return (add l r)
        | Sub (l, r) -> ev e l >>= fun l -> ev e r >>= fun r -> return (sub l r)
        | Mul (l, r) -> ev e l >>= fun l -> ev e r >>= fun r -> return (mul l r)
        | Div (l, r) ->
            ev e r
            >>= fun r ->
            if r = 0L then error "division by zero"
            else ev e l >>= fun l -> return (div l r)
        | Mod (l, r) ->
            ev e r
            >>= fun r ->
            if r = 0L then error "division by zero"
            else ev e l >>= fun l -> return (modulo l r)
        | Shl (l, r) ->
            ev e r
            >>= fun r ->
            if r < 0L then error "shift by a negative value"
            else ev e l >>= fun l -> return (shift_left l (to_int r mod 64))
        | Shr (l, r) ->
            ev e r
            >>= fun r ->
            if r < 0L then error "shift by a negative value"
            else ev e l >>= fun l -> return (shift_right l (to_int r mod 64))
        | Reg _ -> error "expression is not simple or relocatable"
        | Label l -> (
          match MapString.find_opt l e with
          | Some (Lb s) | Some (Ls s) -> return (string_to_int64 s)
          | _ ->
              error "relocation truncated to fit: R_X86_64_8 against `.data\'" )
      in
      function
      | Label l -> (
        match MapString.find_opt l env17 with
        | Some (Lb b) -> return (Lb b)
        | Some (Ls s) -> return (Ls s)
        | _ -> error "relocation truncated to fit: R_X86_64_8 against `.data\'"
        )
      | Reg _ -> error "expression is not simple or relocatable"
      | e -> ev env17 e >>= fun i -> return (Lb (int64_to_string i)) in
    let str_to_lb s = return (Lb s) in
    let env_add id v env9 =
      match MapString.find_opt id env9 with
      | None -> return (MapString.add id v env9)
      | Some _ -> error ("label `" ^ id ^ "\' inconsistently redefined") in
    let env_set id v env9 =
      match MapString.find_opt id env9 with
      | None -> return (MapString.add id v env9)
      | Some _ -> return (MapString.update id (fun _ -> Some v) env9) in
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
            | Lb s1 | Ls s1 ->
                helper s "" (string_to_int64 s1) >>| fun s1 -> Ls s1
            | _ -> error "fatal error" ) in
      function
      | [] ->
          if acc = "" then error "no operand for data declaration"
          else return acc
      | hd :: tl -> (
          helper env16 hd
          >>= function
          | Ls s | Lb s -> pr_init_val env16 (acc ^ s) tl
          | _ -> error "fatal error" ) in
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
    (MapString.singleton "None" (Lb "BA"))
    []

let%test _ =
  succ_prepr
    (Directive [Section ("TEXT", [EqualDir (Id "l", Add (Const 1L, Const 1L))])])
    (MapString.singleton "l" (Lb "\x02"))
    []

let%test _ =
  fail_prepr
    (Directive [Section ("TEXT", [EquDir (Id "l", Add (Label "fgf", Const 1L))])]
    )
    "relocation truncated to fit: R_X86_64_8 against `.data'"
