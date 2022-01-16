open Ast
open Ty
open Format
module BindMap = Map.Make (String)
module BindSet = Set.Make (String)

module type FailBimapMonad = sig
  include Base.Monad.S2

  val fail : 'e -> ('a, 'e) t
  val bimap : ('a, 'e) t -> ok:('a -> 'b) -> err:('e -> 'b) -> 'b
end

module Interpret (M : FailBimapMonad) : sig
  type run_ok
  type run_err

  val pp_run_ok : formatter -> run_ok -> unit
  val pp_run_err : formatter -> run_err -> unit
  val run : (decl * ty) list -> (run_ok, run_err) M.t
end = struct
  open M

  let ( let* ) m f = bind m ~f

  type value =
    | VInt of int
    | VString of string
    | VBool of bool
    | VExc of exc
    | VTuple of value list
    | VList of value list
    | VRef of value ref
    | VFun of ((value, run_err) t Lazy.t -> (value, run_err) t)

  (* refs are used for recursive functions *)
  and env = value option ref BindMap.t

  and run_err =
    | Unbound of string
    | Incorrect_ty of value
    | Exc of exc
    | Non_exhaustive of ptrn list
    | Div0
    | Cmp_fun

  let v_int n = VInt n
  let v_string s = VString s
  let v_bool b = VBool b
  let v_exc exc = VExc exc
  let v_tuple l = VTuple l
  let v_list l = VList l
  let v_ref v = VRef v
  let v_lazy_fun f = VFun f
  let v_fun f = v_lazy_fun (fun x -> Lazy.force x >>= f)
  let v_fun2 f = v_fun (fun x -> return (v_fun (fun y -> f (x, y))))

  let rec pp_value fmt = function
    | VInt n -> fprintf fmt "%d" n
    | VString s -> fprintf fmt "%S" s
    | VBool b -> fprintf fmt "%b" b
    | VExc exc -> pp_exc fmt exc
    | VTuple l ->
      fprintf
        fmt
        "(%a)"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_value)
        l
    | VList l ->
      fprintf
        fmt
        "[%a]"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp_value)
        l
    | VRef v -> fprintf fmt "{contents = %a}" pp_value !v
    | VFun _ -> fprintf fmt "<fun>"
  ;;

  let pp_run_err fmt = function
    | Unbound s -> fprintf fmt "Unbound value %s" s
    | Incorrect_ty v -> fprintf fmt "Value %a has incorrect type" pp_value v
    | Exc e -> fprintf fmt "Exception: %a" pp_exc e
    | Non_exhaustive ps ->
      fprintf
        fmt
        "This pattern-matching is not exhaustive:\n%a"
        (pp_print_list pp_ptrn)
        ps
    | Div0 -> fprintf fmt "Division by zero"
    | Cmp_fun -> fprintf fmt "Compare: functional value"
  ;;

  let pp_env fmt env =
    BindMap.iter
      (fun k v ->
        Format.fprintf fmt "%s=%a\n" k (fun fmt v -> pp_print_option pp_value fmt !v) v)
      env
  ;;

  type run_ok_elm = string * (ty * value)

  let pp_run_ok_elm fmt (name, (ty, value)) =
    Format.fprintf fmt "val %s : %a = %a" name pp_ty ty pp_value value
  ;;

  type run_ok = run_ok_elm list

  let pp_run_ok = pp_print_list ~pp_sep:pp_force_newline pp_run_ok_elm

  let int_binop f =
    v_fun2 (function
        | VInt x, VInt y ->
          (try return (VInt (f x y)) with
          | Division_by_zero -> fail Div0)
        | x, y -> fail (Incorrect_ty (VTuple [ x; y ])))
  ;;

  let rec cmp_value = function
    | VInt x, VInt y -> return (Int.compare x y)
    | VString x, VString y -> return (String.compare x y)
    | VBool x, VBool y -> return (Bool.compare x y)
    | VExc x, VExc y -> return (compare_exc x y)
    | VTuple (x :: xs), VTuple (y :: ys) | VList (x :: xs), VList (y :: ys) ->
      let* cmp = cmp_value (x, y) in
      (match cmp with
      | 0 -> cmp_value (VTuple xs, VTuple ys)
      | _ -> return cmp)
    | VTuple [], VTuple (_ :: _) | VList [], VList (_ :: _) -> return (-1)
    | VTuple (_ :: _), VTuple [] | VList (_ :: _), VList [] -> return 1
    | VTuple [], VTuple [] | VList [], VList [] -> return 0
    | VRef x, VRef y -> cmp_value (!x, !y)
    | VFun _, VFun _ -> fail Cmp_fun
    | x, y -> fail (Incorrect_ty (VTuple [ x; y ]))
  ;;

  let cmp_binop f = v_fun2 (fun p -> cmp_value p >>| fun cmp -> VBool (f cmp 0))

  let bool_binop ~preferred =
    v_fun (function
        | VBool x when x = preferred ->
          return (v_lazy_fun (fun _ -> return (VBool preferred)))
        | x ->
          return
            (v_fun (fun y ->
                 match x, y with
                 | VBool _, VBool b -> return (VBool b)
                 | _, _ -> fail (Incorrect_ty (VTuple [ x; y ])))))
  ;;

  let lookup_val name env =
    match BindMap.find_opt name env with
    | None ->
      (match name with
      | "+" -> return (int_binop ( + ))
      | "-" -> return (int_binop ( - ))
      | "*" -> return (int_binop ( * ))
      | "/" -> return (int_binop ( / ))
      | "=" -> return (cmp_binop ( = ))
      | "!=" -> return (cmp_binop ( != ))
      | "<" -> return (cmp_binop ( < ))
      | "<=" -> return (cmp_binop ( <= ))
      | ">" -> return (cmp_binop ( > ))
      | ">=" -> return (cmp_binop ( >= ))
      | "&&" -> return (bool_binop ~preferred:false)
      | "||" -> return (bool_binop ~preferred:true)
      | ":=" ->
        return
          (v_fun2 (function
              | VRef r, v ->
                r := v;
                return (VTuple [])
              | x, y -> fail (Incorrect_ty (VTuple [ x; y ]))))
      | "::" ->
        return
          (v_fun2 (function
              | hd, VList tl -> return (VList (hd :: tl))
              | x, y -> fail (Incorrect_ty (VTuple [ x; y ]))))
      | "~-" ->
        return
          (v_fun (function
              | VInt x -> return (VInt (-x))
              | x -> fail (Incorrect_ty x)))
      | "~!" ->
        return
          (v_fun (function
              | VRef r -> return !r
              | x -> fail (Incorrect_ty x)))
      | "println" ->
        return
          (v_fun (function
              | VString s ->
                printf "%s\n%!" s;
                return (VTuple [])
              | x -> fail (Incorrect_ty x)))
      | "raise1" ->
        return
          (v_fun (function
              | VTuple [] -> fail (Exc Exc1)
              | x -> fail (Incorrect_ty x)))
      | "raise2" ->
        return
          (v_fun (function
              | VTuple [] -> fail (Exc Exc2)
              | x -> fail (Incorrect_ty x)))
      | "ref" -> return (v_fun (fun v -> return (VRef (ref v))))
      | "sneaky_eff" ->
        return
          (v_fun (function
              | VFun fv -> return (VFun fv)
              | x -> fail (Incorrect_ty x)))
      | _ -> fail (Unbound name))
    | Some r ->
      (match !r with
      | Some v -> return v
      | None -> fail (Unbound name))
  ;;

  let add_val name value env = BindMap.add name (ref (Some value)) env

  (* `case_env` is `None` if `value` doesn't match `ptrn`,
     `case_env` is `Some(fail (Incorrect_ty value))` if `value` has incorrect type,
     `case_env` is `Some(return env)` if `value` matches `ptrn` *)
  let rec case_env value ptrn =
    match ptrn, value with
    | PVal s, _ -> Some (return (add_val s value BindMap.empty))
    | PConst (CInt c), VInt v when c = v -> Some (return BindMap.empty)
    | PConst (CInt _), VInt _ -> None
    | PConst (CString c), VString v when c = v -> Some (return BindMap.empty)
    | PConst (CString _), VString _ -> None
    | PConst (CBool c), VBool v when c = v -> Some (return BindMap.empty)
    | PConst (CBool _), VBool _ -> None
    | PConst CNil, VList [] -> Some (return BindMap.empty)
    | PConst CNil, VList _ -> None
    | PTuple ps, VTuple vs ->
      (match vs, ps with
      | [], [] -> Some (return BindMap.empty)
      | v :: vs, p :: ps ->
        mrg_case_envs (case_env v p) (fun () -> case_env (VTuple vs) (PTuple ps))
      | _ -> Some (fail (Incorrect_ty value)))
    | PCons (ps, pl), VList vs ->
      (match vs, ps with
      | (_ : value list), [] -> case_env value pl
      | v :: vs, p :: ps ->
        mrg_case_envs (case_env v p) (fun () -> case_env (VList vs) (PCons (ps, pl)))
      | _ -> None)
    | _ -> Some (fail (Incorrect_ty value))

  and mrg_case_envs env1 env2 =
    Option.bind env1 (fun env1 ->
        bimap
          env1
          ~ok:(fun env1 ->
            Option.map
              (fun env2 ->
                env2 >>| fun env2 -> BindMap.add_seq (BindMap.to_seq env2) env1)
              (env2 ()))
          ~err:(fun err -> Some (fail err)))
  ;;

  let rec eval expr env =
    match expr with
    | EVal name -> lookup_val name env
    | EConst (CInt n) -> return (v_int n)
    | EConst (CString s) -> return (v_string s)
    | EConst (CBool b) -> return (v_bool b)
    | EConst CNil -> return (v_list [])
    | EApp (fn, arg) ->
      eval fn env
      >>= (function
      | VFun fv -> fv (lazy (eval arg env))
      | v -> fail (Incorrect_ty v))
    | ETuple l -> all (List.map (fun e -> eval e env) l) >>| v_tuple
    | ELet (decl, expr) -> add_decl decl env >>= eval expr
    | EMatch (scr, cases) ->
      eval scr env
      >>= fun value ->
      (match
         List.find_map
           (fun (ptrn, body) -> Option.map (fun env -> env, body) (case_env value ptrn))
           cases
       with
      | Some (case_env, body) ->
        case_env
        >>= fun case_env -> eval body (BindMap.add_seq (BindMap.to_seq case_env) env)
      | None -> fail (Non_exhaustive (List.map (fun (ptrn, _) -> ptrn) cases)))
    | EFun (prm, body) -> return (v_fun (fun arg -> eval body (add_val prm arg env)))
    | ETry (scr, excs) ->
      bimap
        (eval scr env)
        ~ok:(fun v -> return v)
        ~err:(function
          | Exc exc ->
            (match List.find_opt (fun (e, _) -> equal_exc exc e) excs with
            | None -> fail (Exc exc)
            | Some (_, expr) -> eval expr env)
          | err -> fail err)

  and add_decl decl env =
    if decl.is_rec
    then (
      let ref = ref None in
      let env = BindMap.add decl.name ref env in
      eval decl.expr env
      >>| fun v ->
      ref := Some v;
      env)
    else eval decl.expr env >>| fun v -> add_val decl.name v env
  ;;

  let run p =
    List.fold_left
      (fun acc (decl, t) ->
        acc
        >>= fun (env, vals) ->
        add_decl decl env
        >>= fun env ->
        lookup_val decl.name env
        >>| fun v -> env, (decl.name, (t, v)) :: List.remove_assoc decl.name vals)
      (return (BindMap.empty, []))
      p
    >>| fun (_, res) -> List.rev res
  ;;
end

module InterpretResult = Interpret (struct
  include Base.Result

  let bimap res ~ok ~err =
    match res with
    | Ok v -> ok v
    | Error v -> err v
  ;;
end)
