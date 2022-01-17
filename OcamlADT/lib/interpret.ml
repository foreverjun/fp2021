open Format
open Ast
open Parser

module type MONAD_FAIL = sig
  include Base.Monad.S2

  val fail : 'e -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

module BindsMap = Map.Make (String)

type value =
  | VString of string
  | VBool of bool
  | VInt of int
  | VList of value list
  | Vtuple of value list
  | VFun of pattern * expr * env
  | VAdt of string * value
[@@deriving eq]

and env = value option ref BindsMap.t

let vint i = VInt i
let vstring s = VString s
let vbool b = VBool b
let vtuple t = Vtuple t
let vlist l = VList l
let vadt s v = VAdt (s, v)

let rec pp_value fmt = function
  | VInt i -> fprintf fmt "%d" i
  | VString str -> fprintf fmt "%s" str
  | VBool b -> fprintf fmt "%b" b
  | Vtuple t ->
      fprintf fmt "(%a)"
        (pp_print_list ~pp_sep:(fun _ _ -> fprintf fmt ", ") pp_value)
        t
  | VList l ->
      fprintf fmt "[%a]"
        (pp_print_list ~pp_sep:(fun _ _ -> fprintf fmt "; ") pp_value)
        l
  | VFun _ -> fprintf fmt "<fun>"
  | VAdt (constr, value) -> fprintf fmt "%s %a" constr pp_value value

type interpret_err =
  | Division_by_zero
  | Unbound of string
  | Non_exhaust
  | Incorrect_eval of value
[@@deriving eq, show {with_path= false}]

type decl_binding = id * value [@@deriving eq]
type interpret_ok = decl_binding list [@@deriving eq]

let pp_interpret_err fmt = function
  | Unbound str -> fprintf fmt "Unbound value %s" str
  | Division_by_zero -> fprintf fmt "Division by zero"
  | Non_exhaust -> fprintf fmt "This pattern matching is not exhaustive"
  | Incorrect_eval value ->
      fprintf fmt "Value %a is of incorrect type" pp_value value

let pp_decl_binding fmt (name, value) =
  fprintf fmt "val %s = %a" name pp_value value

let pp_interpret_ok = pp_print_list ~pp_sep:pp_force_newline pp_decl_binding

module Interpret (M : MONAD_FAIL) : sig
  val run : program -> (interpret_ok, interpret_err) M.t
end = struct
  open M

  let look_for_bind map name =
    try
      match !(BindsMap.find name map) with
      | Some v -> return v
      | None -> fail (Unbound name)
    with Not_found -> fail (Unbound name)

  let add_bind id value env = BindsMap.add id (ref (Some value)) env

  let extend_env env binds =
    List.fold_left (fun env (id, v) -> add_bind id v env) env binds

  let rec pattern_bindings = function
    | PWild | PNil | PConst _ -> []
    | PVar name -> [name]
    | PTuple pts -> List.concat_map (fun x -> pattern_bindings x) pts
    | PCons (patt1, patt2) -> pattern_bindings patt1 @ pattern_bindings patt2
    | PACase (_, pcase) -> pattern_bindings pcase

  let rec pattern_decl_bindings pattern value =
    match (pattern, value) with
    | PNil, VList [] -> return []
    | PWild, _ -> return []
    | PVar name, v -> return [(name, v)]
    | PConst (CInt c), VInt v when c = v -> return []
    | PConst (CInt _), VInt _ -> fail Non_exhaust
    | PConst (CString c), VString v when c = v -> return []
    | PConst (CString _), VString _ -> fail Non_exhaust
    | PConst (CBool c), VBool v when c = v -> return []
    | PConst (CBool _), VBool _ -> fail Non_exhaust
    | PCons (patt1, patt2), VList (hd :: tl) ->
        pattern_decl_bindings patt1 hd
        >>= fun hd_match ->
        pattern_decl_bindings patt2 (VList tl)
        >>= fun tl_match -> return (hd_match @ tl_match)
    | PTuple pts, Vtuple vts -> (
      match (pts, vts) with
      | [], [] -> return []
      | hd_pts :: tl_pts, hd_vts :: tl_vts ->
          pattern_decl_bindings hd_pts hd_vts
          >>= fun bind_hd ->
          pattern_decl_bindings (PTuple tl_pts) (Vtuple tl_vts)
          >>= fun bind_tl -> return (bind_hd @ bind_tl)
      | _ -> fail Non_exhaust )
    | PACase (pconstr, p), VAdt (vconstr, v) when pconstr = vconstr ->
        pattern_decl_bindings p v
    | _ -> fail Non_exhaust

  let rec check_pattern pattern value =
    match (pattern, value) with
    | PVar _, _ -> true
    | PWild, _ -> true
    | PConst (CInt pn), VInt vn -> pn = vn
    | PConst (CString pn), VString vn -> pn = vn
    | PConst (CBool pn), VBool vn -> pn = vn
    | PCons (phd, ptl), VList (vhd :: vtl) ->
        check_pattern phd vhd && check_pattern ptl (VList vtl)
    | PNil, VList [] -> true
    | PTuple ps, Vtuple vs ->
        List.length ps = List.length vs
        && List.for_all2 (fun p v -> check_pattern p v) ps vs
    | PACase (pconstr, p), VAdt (vconstr, v) ->
        pconstr = vconstr && check_pattern p v
    | _ -> false

  let rec eval expr env =
    match expr with
    | EVar name -> look_for_bind env name
    | EConst c -> (
      match c with
      | CInt i -> return (vint i)
      | CString str -> return (vstring str)
      | CBool b -> return (vbool b) )
    | ENil -> return (vlist [])
    | EBinOp (op, e1, e2) -> (
        let* value1 = eval e1 env in
        match (value1, op) with
        | VBool false, And -> return (vbool false)
        | VBool true, Or -> return (vbool true)
        | _ -> (
            let* value2 = eval e2 env in
            match (value1, value2, op) with
            | VInt x, VInt y, Add -> return (vint (x + y))
            | VInt x, VInt y, Sub -> return (vint (x - y))
            | VInt x, VInt y, Mul -> return (vint (x * y))
            | VInt _, VInt y, Div when y = 0 -> fail Division_by_zero
            | VInt x, VInt y, Div -> return (vint (x / y))
            | VInt x, VInt y, Less -> return (vbool (x < y))
            | VInt x, VInt y, Leq -> return (vbool (x <= y))
            | VInt x, VInt y, Gre -> return (vbool (x > y))
            | VInt x, VInt y, Geq -> return (vbool (x >= y))
            | VInt x, VInt y, Eq -> return (vbool (x = y))
            | VString str1, VString str2, Eq -> return (vbool (str1 = str2))
            | VBool b1, VBool b2, Eq -> return (vbool (b1 = b2))
            | VInt x, VInt y, Neq -> return (vbool (x != y))
            | VString str1, VString str2, Neq -> return (vbool (str1 != str2))
            | VBool b1, VBool b2, Neq -> return (vbool (b1 != b2))
            | VBool _, VBool b2, And | VBool _, VBool b2, Or ->
                return (vbool b2)
            | _ -> fail (Incorrect_eval (Vtuple [value1; value2])) ) )
    | ETuple l -> all (List.map (fun e -> eval e env) l) >>| vtuple
    | EIf (e1, e2, e3) -> (
        let* res = eval e1 env in
        match res with
        | VBool true -> eval e2 env
        | VBool false -> eval e3 env
        | e -> fail (Incorrect_eval e) )
    | EFun (pattern, expr) -> return (VFun (pattern, expr, env))
    | ECons (expr1, expr2) -> (
        let* value1 = eval expr1 env in
        let* value2 = eval expr2 env in
        match value2 with
        | VList l -> return (VList (value1 :: l))
        | _ -> fail (Incorrect_eval value2) )
    | EApp (fn_expr, arg_expr) -> (
        let* fn_value = eval fn_expr env in
        let* arg_value = eval arg_expr env in
        match fn_value with
        | VFun (pattern, expr, env) ->
            let* binds = pattern_decl_bindings pattern arg_value in
            eval expr (extend_env env binds)
        | _ -> fail (Incorrect_eval fn_value) )
    | ELet (binding, expr) ->
        let* env = add_binding binding env in
        eval expr env
    | EMatch (expr, cases) -> (
        let* value = eval expr env in
        match
          List.find_opt (fun (pattern, _) -> check_pattern pattern value) cases
        with
        | None -> fail Non_exhaust
        | Some (pattern, expr) ->
            let* binds = pattern_decl_bindings pattern value in
            eval expr (extend_env env binds) )
    | EConstr (constr, expr) ->
        let* value = eval expr env in
        return (vadt constr value)

  and add_binding binding env =
    match binding with
    | false, pattern, expr ->
        let* value = eval expr env in
        let* binds = pattern_decl_bindings pattern value in
        return (extend_env env binds)
    | true, pattern, expr ->
        let binds = pattern_bindings pattern in
        let env =
          List.fold_left
            (fun env id -> BindsMap.add id (ref None) env)
            env binds in
        let* value = eval expr env in
        let* binds = pattern_decl_bindings pattern value in
        List.iter (fun (id, value) -> BindsMap.find id env := Some value) binds;
        return env

  let run program =
    let* binds, _ =
      List.fold_left
        (fun acc -> function
          | DAdt _ -> acc
          | DLet binding ->
              let* binds, env = acc in
              let* env = add_binding binding env in
              let _, pattern, _ = binding in
              let* new_binds =
                all
                  (List.map
                     (fun id ->
                       let* value = look_for_bind env id in
                       return (id, value) )
                     (pattern_bindings pattern) ) in
              let binds =
                List.fold_left
                  (fun binds (id, _) -> List.remove_assq id binds)
                  binds new_binds in
              return (binds @ new_binds, env) )
        (return ([], BindsMap.empty))
        program in
    return binds
end

module InterpretResult = Interpret (struct
  include Base.Result

  let ( let* ) m f = bind m ~f
end)

let run_interpret code = InterpretResult.run (parse_or_error code)

let pp_run_interpret fmt code =
  match run_interpret code with
  | Ok ok -> fprintf fmt "%a\n" pp_interpret_ok ok
  | Error err -> fprintf fmt "Error:\n%a\n" pp_interpret_err err

(* TESTS *)

let test_run_interpret code expected =
  match run_interpret code with
  | Ok ok -> (
      let flag =
        List.exists
          (fun needed -> List.for_all (fun x -> equal_decl_binding needed x) ok)
          expected in
      match flag with
      | true -> true
      | false ->
          printf "Error\nExpected:%a\nFound:%a\n" pp_interpret_ok expected
            pp_interpret_ok ok;
          false )
  | Error err ->
      printf "%a\n" pp_interpret_err err;
      false

let test_run_interpret_err code expected =
  match run_interpret code with
  | Ok ok ->
      printf "Didn't find error: \n%a\n" pp_interpret_ok ok;
      false
  | Error err when equal_interpret_err err expected -> true
  | Error err ->
      printf "False error, expected: %a, found %a\n" pp_interpret_err expected
        pp_interpret_err err;
      false

let%test _ = test_run_interpret {|
  let x = 2
|} [("x", VInt 2)]

let%test _ =
  test_run_interpret
    {|
  type a = 
  | Age of int 
  | Name of string
  ;;
  
  let x = Age 2
|}
    [("x", VAdt ("Age", VInt 2))]

let%test _ = test_run_interpret {|
  let x = 0 + 9090 / 3
|} [("x", VInt 3030)]

let%test _ =
  test_run_interpret
    {|
     let y = if 5 > 4 then true else false
   |}
    [("y", VBool true)]

let%test _ =
  test_run_interpret
    {|
     let y = if 3 > 4 then true else false
   |}
    [("y", VBool false)]

let%test _ = test_run_interpret_err {|
  let x = 1 / 0
|} Division_by_zero

let%test _ = test_run_interpret_err {|
  let y = x
|} (Unbound "x")

let%test _ =
  test_run_interpret_err {|
  let x = 1 :: 2 :: "p"
|}
    (Incorrect_eval (VString "p"))
