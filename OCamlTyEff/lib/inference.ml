open Base
open Ast
open Ty
module Format = Caml.Format (* silencing a warning *)

open Caml.Format
module IntMap = Caml.Map.Make (Int)

type error =
  | OccursCheck
  | NoVariable of string
  | UnificationFailed of ty * ty
  | UnhandledEff of eff_set
  | PtrnRebound of string
  | IllegalRec of string

let pp_error ppf : error -> _ = function
  | OccursCheck -> fprintf ppf "Occurs check failed"
  | NoVariable s -> fprintf ppf "Undefined variable '%s'" s
  | UnificationFailed (l, r) ->
    fprintf ppf "Unification failed on %a and %a" pp_ty l pp_ty r
  | UnhandledEff eff -> fprintf ppf "Unhandled effects %a" pp_eff_set eff
  | PtrnRebound s -> fprintf ppf "Variable '%s' is bound several times in the matching" s
  | IllegalRec s ->
    fprintf
      ppf
      "This kind of expression is not allowed as right-hand side of `let rec %s`"
      s
;;

module R : sig
  type 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RList : sig
    val fold_left : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
    val iter : 'a list -> f:('a -> unit t) -> unit t
  end

  (** Creation of a fresh name from internal state *)
  val fresh : int t

  val get_weak_s : subst t
  val set_weak_s : subst -> unit t

  (** Running a transformer: getting the inner result value *)
  val run : 'a t -> ('a, error) Result.t
end = struct
  (* A compositon: State monad after Result monad *)
  type 'a t = subst * int -> (subst * int) * ('a, error) Result.t

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
   fun m f st ->
    let last, r = m st in
    match r with
    | Result.Error x -> last, Error x
    | Ok a -> f a last
 ;;

  let fail e st = st, Result.fail e
  let return x last = last, Result.return x
  let bind x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
   fun x f st ->
    match x st with
    | st, Ok x -> st, Ok (f x)
    | st, Result.Error e -> st, Result.Error e
 ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  module RList = struct
    let fold_left xs ~init ~f =
      Base.List.fold_left xs ~init ~f:(fun acc x ->
          let open Syntax in
          let* acc = acc in
          f acc x)
    ;;

    let iter xs ~f = fold_left xs ~init:(return ()) ~f:(fun () -> f)
  end

  let fresh (s, last) = (s, last + 1), Result.Ok last
  let get_weak_s (s, last) = (s, last), Result.Ok s
  let set_weak_s s (_, last) = (s, last), Result.Ok ()
  let run m = snd (m ((Weak, [], []), 0))
end

let fresh_var kind =
  let open R in
  fresh >>| fun n -> TVar (kind, n)
;;

let fresh_var_eff kind =
  let open R in
  fresh >>| fun n -> EffVar (kind, n)
;;

type fresh = int

module EffSet = struct
  include EffSet

  let concrete_effs effs =
    filter
      (function
        | EffVar _ -> false
        | _ -> true)
      effs
  ;;

  let var_effs effs =
    Caml.List.of_seq
      (Seq.filter_map
         (function
           | EffVar (k, b) -> Some (k, b)
           | _ -> None)
         (to_seq effs))
  ;;

  let split_effs effs = concrete_effs effs, var_effs effs
  let split_vars vs = List.partition_tf vs ~f:(fun (k, _) -> equal_var_kind k Weak)

  let free_vars kind effs =
    let wv, nv = split_vars (var_effs effs) in
    VarSet.of_list
      (List.map
         ~f:snd
         (match kind with
         | Weak -> wv
         | Normal -> nv))
  ;;
end

module Type = struct
  type t = ty

  let free_vars kind =
    let rec helper acc = function
      | TInt | TString | TBool | TExc _ -> acc
      | TTuple l -> List.fold_left l ~init:acc ~f:helper
      | TList t | TRef t -> helper acc t
      | TVar (vk, b) when equal_var_kind vk kind -> VarSet.add b acc
      | TVar _ -> acc
      | TFun (l, _, r) -> helper (helper acc l) r
    in
    helper VarSet.empty
  ;;

  let free_vars_eff_counted kind =
    let rec helper acc = function
      | TInt | TString | TBool | TExc _ | TVar _ -> acc
      | TTuple l -> List.fold_left l ~init:acc ~f:helper
      | TList t | TRef t -> helper acc t
      | TFun (l, eff, r) ->
        helper
          (helper
             (VarSet.fold
                (fun b acc ->
                  IntMap.add b (1 + Option.value ~default:0 (IntMap.find_opt b acc)) acc)
                (EffSet.free_vars kind eff)
                acc)
             l)
          r
    in
    helper IntMap.empty
  ;;

  let free_vars_eff kind t =
    VarSet.of_seq (Seq.map fst (IntMap.to_seq (free_vars_eff_counted kind t)))
  ;;

  let useful_free_vars_eff kind =
    let rec helper acc = function
      | TFun (l, _, r) -> VarSet.union (free_vars_eff kind l) (helper acc r)
      | other -> VarSet.union acc (free_vars_eff kind other)
    in
    helper VarSet.empty
  ;;

  let occurs_in kind v t = VarSet.mem v (free_vars kind t)
end

module VarSet = struct
  include VarSet

  let fold_R f acc set =
    fold
      (fun x acc ->
        let open R.Syntax in
        let* acc = acc in
        f acc x)
      acc
      set
  ;;
end

module Subst = struct
  open R
  open R.Syntax

  type t = subst

  let pp ppf (kind, subst, eff_subst) =
    let open Format in
    fprintf
      ppf
      "[ %a; %a ]"
      (pp_print_list
         ~pp_sep:(fun ppf () -> fprintf ppf ", ")
         (fun ppf (k, v) -> fprintf ppf "%a%d -> %a" pp_var_kind kind k pp_ty v))
      subst
      (pp_print_list
         ~pp_sep:(fun ppf () -> fprintf ppf ", ")
         (fun ppf (k, v) -> fprintf ppf "%a%d -> %a" pp_var_kind kind k pp_eff_set v))
      eff_subst
  ;;

  let empty kind = kind, [], []

  let mapping kind k v =
    if Type.occurs_in kind k v then fail OccursCheck else return (k, v)
  ;;

  let singleton kind k v =
    let* mapping = mapping kind k v in
    return (kind, [ mapping ], [])
  ;;

  let mapping_eff kind k v = return (k, EffSet.remove (EffVar (kind, k)) v)

  let singleton_eff kind k v =
    let* mapping = mapping_eff kind k v in
    return (kind, [], [ mapping ])
  ;;

  let find_exn k xs = List.Assoc.find_exn xs k ~equal:Int.equal
  let find k xs = List.Assoc.find xs k ~equal:Int.equal
  let remove xs k = List.Assoc.remove xs k ~equal:Int.equal

  let apply_eff (sk, _, eff_s) effs =
    EffSet.of_seq
      (Seq.flat_map
         (function
           | EffVar (vk, b) when equal_var_kind vk sk ->
             (match find_exn b eff_s with
             | exception Not_found_s _ -> Seq.return (EffVar (vk, b))
             | x -> EffSet.to_seq x)
           | x -> Seq.return x)
         (EffSet.to_seq effs))
  ;;

  let apply (sk, s, eff_s) =
    let rec helper t =
      match t with
      | TInt | TString | TBool | TExc _ -> t
      | TTuple l -> TTuple (List.map l ~f:helper)
      | TList x -> TList (helper x)
      | TRef x -> TRef (helper x)
      | TVar (vk, b) when equal_var_kind sk vk ->
        (match find_exn b s with
        | exception Not_found_s _ -> t
        | x -> x)
      | TVar _ -> t
      | TFun (l, e, r) -> TFun (helper l, apply_eff (sk, s, eff_s) e, helper r)
    in
    helper
  ;;

  let rec unify_eff k =
    let fv vs =
      let wv, _ = EffSet.split_vars vs in
      let* fv = fresh_var_eff (if List.is_empty wv then Normal else Weak) in
      return (EffSet.singleton fv)
    in
    let subs v eff =
      if List.is_empty v && not (EffSet.is_empty eff)
      then fail (UnhandledEff eff)
      else (
        let wv, nv = EffSet.split_vars v in
        let* ws =
          RList.fold_left
            wv
            ~init:
              (match k with
              | Normal -> get_weak_s
              | Weak -> return (empty Weak))
            ~f:(fun ws (_, b) -> extend_eff ws (b, eff))
        in
        match k with
        | Weak -> return ws
        | Normal ->
          let* () = set_weak_s ws in
          RList.fold_left
            nv
            ~init:(return (empty Normal))
            ~f:(fun ns (_, b) -> extend_eff ns (b, eff)))
    in
    fun l r ->
      if equal_eff_set l r
      then return (empty k)
      else
        let* ws = get_weak_s in
        let l, r = apply_eff ws l, apply_eff ws r in
        let vm = EffSet.var_effs (EffSet.inter l r) in
        let l, r = EffSet.diff l r, EffSet.diff r l in
        let cl, vl = EffSet.split_effs l in
        let cr, vr = EffSet.split_effs r in
        match vm with
        | _ :: _ ->
          let* fv = fv vm in
          subs vm (EffSet.union fv (EffSet.union l r))
        | [] ->
          let* fv =
            match vl, vr with
            | _ :: _, _ :: _ -> fv (vl @ vr)
            | _ -> return EffSet.empty
          in
          let* s1 = subs vl (EffSet.union fv cr) in
          let* s2 = subs vr (EffSet.union fv cl) in
          compose s1 s2

  and extend_common (kind, s, s_eff) s2 =
    let* s2 =
      RList.fold_left s ~init:(return s2) ~f:(fun (_, acc, acc_eff) (k, v) ->
          let v = apply s2 v in
          let* mapping = mapping kind k v in
          return (kind, mapping :: acc, acc_eff))
    in
    RList.fold_left s_eff ~init:(return s2) ~f:(fun (_, acc, acc_eff) (k, v) ->
        let v = apply_eff s2 v in
        let* mapping = mapping_eff kind k v in
        return (kind, acc, mapping :: acc_eff))

  and extend_eff (kind, s, s_eff) (k, v) =
    match List.Assoc.find s_eff ~equal:Int.equal k with
    | None ->
      let v = apply_eff (kind, s, s_eff) v in
      let* s2 = singleton_eff kind k v in
      extend_common (kind, s, s_eff) s2
    | Some v2 ->
      let* s2 = unify_eff kind v v2 in
      compose (kind, s, s_eff) s2

  and unify k l r =
    if equal_ty l r
    then return (empty k)
    else (
      match l, r with
      | TTuple (hd1 :: tl1), TTuple (hd2 :: tl2) ->
        let* subs1 = unify k hd1 hd2 in
        let* subs2 = unify k (apply subs1 (TTuple tl1)) (apply subs1 (TTuple tl2)) in
        compose subs1 subs2
      | TList a, TList b | TRef a, TRef b -> unify k a b
      | TVar (vk, b), t when equal_var_kind k vk -> singleton k b t
      | t, TVar (vk, b) when equal_var_kind k vk -> singleton k b t
      | TVar (Weak, b), t | t, TVar (Weak, b) ->
        let* n_s_ty =
          VarSet.fold_R
            (fun acc v ->
              let* fv = fresh_var Weak in
              return ((v, fv) :: acc))
            (Type.free_vars Normal t)
            (return [])
        in
        let* n_s_eff =
          VarSet.fold_R
            (fun acc v ->
              let* fv = fresh_var_eff Weak in
              return ((v, EffSet.singleton fv) :: acc))
            (Type.free_vars_eff Normal t)
            (return [])
        in
        let ns = Normal, n_s_ty, n_s_eff in
        let* ws = get_weak_s in
        let* ws = extend ws (b, apply ns t) in
        let* () = set_weak_s ws in
        return ns
      | TFun (l1, e1, r1), TFun (l2, e2, r2) ->
        let* subs0 = unify_eff k e1 e2 in
        let* subs1 = unify k (apply subs0 l1) (apply subs0 l2) in
        let* subs1 = compose subs0 subs1 in
        let* subs2 = unify k (apply subs1 r1) (apply subs1 r2) in
        compose subs1 subs2
      | _ -> fail (UnificationFailed (l, r)))

  and extend (kind, s, s_eff) (k, v) =
    match List.Assoc.find s ~equal:Int.equal k with
    | None ->
      let v = apply (kind, s, s_eff) v in
      let* s2 = singleton kind k v in
      extend_common (kind, s, s_eff) s2
    | Some v2 ->
      let* s2 = unify kind v v2 in
      compose (kind, s, s_eff) s2

  and compose (kind, s1, s1_effs) (_, s2, s2_effs) =
    let* s = RList.fold_left s2 ~init:(return (kind, s1, s1_effs)) ~f:extend in
    RList.fold_left s2_effs ~init:(return s) ~f:extend_eff
  ;;

  let compose_all ss = RList.fold_left ss ~init:(return (empty Normal)) ~f:compose
end

module Scheme = struct
  type t = scheme

  let free_vars kind = function
    | S (bs, _, t) -> VarSet.diff (Type.free_vars kind t) bs
  ;;

  let free_vars_eff kind = function
    | S (_, bs_eff, t) -> VarSet.diff (Type.free_vars_eff kind t) bs_eff
  ;;

  let apply (kind, sub, sub_eff) (S (names, names_eff, ty)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove s k) names sub in
    let s2_eff = VarSet.fold (fun k s -> Subst.remove s k) names_eff sub_eff in
    S (names, names_eff, Subst.apply (kind, s2, s2_eff) ty)
  ;;

  let pp = pp_scheme
end

module TypeEnv = struct
  type t = (string * scheme) list

  let extend e h = h :: e
  let empty = []

  let free_vars kind env =
    List.fold_left
      ~init:VarSet.empty
      ~f:(fun acc (_, s) -> VarSet.union acc (Scheme.free_vars kind s))
      env
  ;;

  let free_vars_eff kind env =
    List.fold_left
      ~init:VarSet.empty
      ~f:(fun acc (_, s) -> VarSet.union acc (Scheme.free_vars_eff kind s))
      env
  ;;

  let apply s env = List.Assoc.map env ~f:(Scheme.apply s)

  let pp ppf xs =
    Caml.Format.fprintf ppf "{| ";
    List.iter xs ~f:(fun (n, s) -> Caml.Format.fprintf ppf "%s -> %a; " n pp_scheme s);
    Caml.Format.fprintf ppf "|}%!"
  ;;

  let find_exn name xs = List.Assoc.find_exn ~equal:String.equal xs name
end

open R
open R.Syntax

let unify = Subst.unify Normal

let instantiate : scheme -> ty R.t =
 fun (S (bs, bs_eff, t)) ->
  VarSet.fold_R
    (fun typ name ->
      let* f1 = fresh_var_eff Normal in
      let* s = Subst.singleton_eff Normal name (EffSet.singleton f1) in
      return @@ Subst.apply s typ)
    bs_eff
    (VarSet.fold_R
       (fun typ name ->
         let* f1 = fresh_var Normal in
         let* s = Subst.singleton Normal name f1 in
         return @@ Subst.apply s typ)
       bs
       (return t))
;;

let generalize : TypeEnv.t -> Type.t -> Scheme.t =
 fun env ty ->
  let free = VarSet.diff (Type.free_vars Normal ty) (TypeEnv.free_vars Normal env) in
  let free_eff =
    VarSet.diff (Type.free_vars_eff Normal ty) (TypeEnv.free_vars_eff Normal env)
  in
  S (free, free_eff, ty)
;;

let weaken env t =
  VarSet.fold_R
    (fun (_, acc, acc_eff) v ->
      let* fv = fresh_var_eff Weak in
      return (Normal, acc, (v, EffSet.singleton fv) :: acc_eff))
    (VarSet.diff (Type.free_vars_eff Normal t) (TypeEnv.free_vars_eff Normal env))
    (VarSet.fold_R
       (fun (_, acc, acc_eff) v ->
         let* fv = fresh_var Weak in
         return (Normal, (v, fv) :: acc, acc_eff))
       (VarSet.diff (Type.free_vars Normal t) (TypeEnv.free_vars Normal env))
       (return (Subst.empty Normal)))
;;

let rec weaken_res env t =
  match t with
  | TRef _ -> weaken env t
  | TFun (l, _, r) ->
    let* s1 = weaken env l in
    let* s2 = weaken_res env (Subst.apply s1 r) in
    Subst.compose s1 s2
  | TInt | TString | TBool | TExc _ | TVar _ -> return (Subst.empty Normal)
  | TList x -> weaken_res env x
  | TTuple l ->
    RList.fold_left
      l
      ~init:(return (Subst.empty Normal))
      ~f:(fun acc x ->
        let* s = weaken_res env x in
        Subst.compose acc s)
;;

let weaken_res env t =
  let* s = weaken_res env t in
  return (Subst.apply s t)
;;

let unweaken env t =
  let* ws = get_weak_s in
  let t = Subst.apply ws t in
  let env = TypeEnv.apply ws env in
  let* s =
    VarSet.fold_R
      (fun (_, acc, acc_eff) v ->
        let* fv = fresh_var_eff Normal in
        return (Weak, acc, (v, EffSet.singleton fv) :: acc_eff))
      (VarSet.diff (Type.free_vars_eff Weak t) (TypeEnv.free_vars_eff Weak env))
      (VarSet.fold_R
         (fun (_, acc, acc_eff) v ->
           let* fv = fresh_var Normal in
           return (Weak, (v, fv) :: acc, acc_eff))
         (VarSet.diff (Type.free_vars Weak t) (TypeEnv.free_vars Weak env))
         (return (Subst.empty Normal)))
  in
  return (Subst.apply s t)
;;

let lookup_env e xs =
  match List.Assoc.find_exn xs ~equal:String.equal e with
  | (exception Caml.Not_found) | (exception Not_found_s _) ->
    (match e with
    | "+" | "-" | "*" | "/" ->
      let* e = fresh_var_eff Normal in
      let* e2 = fresh_var_eff Normal in
      return (TFun (TInt, EffSet.singleton e, TFun (TInt, EffSet.singleton e2, TInt)))
    | "=" | "!=" | "<" | "<=" | ">" | ">=" ->
      let* tv = fresh_var Normal in
      let* e = fresh_var_eff Normal in
      let* e2 = fresh_var_eff Normal in
      return (TFun (tv, EffSet.singleton e, TFun (tv, EffSet.singleton e2, TBool)))
    | "&&" | "||" ->
      let* e = fresh_var_eff Normal in
      let* e2 = fresh_var_eff Normal in
      return (TFun (TBool, EffSet.singleton e, TFun (TBool, EffSet.singleton e2, TBool)))
    | ":=" ->
      let* tv = fresh_var Normal in
      let* e = fresh_var_eff Normal in
      let* e2 = fresh_var_eff Normal in
      return
        (TFun
           ( TRef tv
           , EffSet.of_list [ EffAsgmt; e ]
           , TFun (tv, EffSet.singleton e2, TTuple []) ))
    | "::" ->
      let* tv = fresh_var Normal in
      let* e = fresh_var_eff Normal in
      let* e2 = fresh_var_eff Normal in
      return
        (TFun (tv, EffSet.singleton e, TFun (TList tv, EffSet.singleton e2, TList tv)))
    | "~-" ->
      let* e = fresh_var_eff Normal in
      return (TFun (TInt, EffSet.singleton e, TInt))
    | "~!" ->
      let* tv = fresh_var Normal in
      let* e = fresh_var_eff Normal in
      return (TFun (TRef tv, EffSet.singleton e, tv))
    | "println" ->
      let* e = fresh_var_eff Normal in
      return (TFun (TString, EffSet.of_list [ EffIO; e ], TTuple []))
    | "raise1" ->
      let* tv = fresh_var Normal in
      let* e = fresh_var_eff Normal in
      return (TFun (TTuple [], EffSet.of_list [ EffExc Exc1; e ], tv))
    | "raise2" ->
      let* tv = fresh_var Normal in
      let* e = fresh_var_eff Normal in
      return (TFun (TTuple [], EffSet.of_list [ EffExc Exc2; e ], tv))
    | "ref" ->
      let* tv = fresh_var Normal in
      let* e = fresh_var_eff Normal in
      return (TFun (tv, EffSet.singleton e, TRef tv))
    | "sneaky_eff" ->
      let* tv1 = fresh_var Normal in
      let* tv2 = fresh_var Normal in
      let* e = fresh_var_eff Normal in
      let* e1 = fresh_var_eff Normal in
      let* e2 = fresh_var_eff Normal in
      return
        (TFun
           ( TFun (tv1, EffSet.singleton e1, tv2)
           , EffSet.singleton e
           , TFun (tv1, EffSet.singleton e2, tv2) ))
    | _ -> fail (NoVariable e))
  | scheme -> instantiate scheme
;;

let pp_env subst ppf env =
  let env : TypeEnv.t =
    List.map
      ~f:(fun (k, S (args, args_eff, v)) -> k, S (args, args_eff, Subst.apply subst v))
      env
  in
  TypeEnv.pp ppf env
;;

let infer_ptrn =
  let rec helper env = function
    | PVal b when List.Assoc.mem env ~equal:String.equal b -> fail (PtrnRebound b)
    | PVal b ->
      let* tv = fresh_var Normal in
      return (TypeEnv.extend env (b, S (VarSet.empty, VarSet.empty, tv)), tv)
    | PConst (CInt _) -> return (env, TInt)
    | PConst (CBool _) -> return (env, TBool)
    | PConst (CString _) -> return (env, TString)
    | PConst CNil ->
      let* tv = fresh_var Normal in
      return (env, TList tv)
    | PTuple ps ->
      let* env, ts =
        RList.fold_left
          ps
          ~init:(return (env, []))
          ~f:(fun (env, ts) p ->
            let* env, t = helper env p in
            return (env, t :: ts))
      in
      return (env, TTuple ts)
    | PCons ([], p) ->
      let* env, t = helper env p in
      let* tv = fresh_var Normal in
      let* s = unify (TList tv) t in
      return (TypeEnv.apply s env, Subst.apply s t)
    | PCons (hd :: tl, tl2) ->
      let* env, hdt = helper env hd in
      let* env, tlt = helper env (PCons (tl, tl2)) in
      let* s = unify (TList hdt) tlt in
      return (TypeEnv.apply s env, tlt)
  in
  helper TypeEnv.empty
;;

let infer =
  let rec (helper : TypeEnv.t -> expr -> (Subst.t * ty * eff_set) R.t) =
   fun env expr ->
    (match expr with
    | EConst (CInt _) -> return (Subst.empty Normal, TInt, EffSet.empty)
    | EConst (CBool _) -> return (Subst.empty Normal, TBool, EffSet.empty)
    | EConst (CString _) -> return (Subst.empty Normal, TString, EffSet.empty)
    | EConst CNil ->
      let* tv = fresh_var Normal in
      return (Subst.empty Normal, TList tv, EffSet.empty)
    | EVal x ->
      let* t = lookup_env x env in
      return (Subst.empty Normal, t, EffSet.empty)
    | EApp (e1, e2) ->
      let* s1, t1, eff1 = helper env e1 in
      let* s2, t2, eff2 = helper env e2 in
      let* tv = fresh_var Normal in
      let* ev = fresh_var_eff Normal in
      let* s3 = unify (Subst.apply s2 t1) (TFun (t2, EffSet.singleton ev, tv)) in
      let trez = Subst.apply s3 tv in
      let erez = Subst.apply_eff s3 (EffSet.singleton ev) in
      let* final_subst = Subst.compose_all [ s3; s2; s1 ] in
      let env = TypeEnv.apply final_subst env in
      let erez = Subst.apply_eff final_subst erez in
      let* trez = weaken_res (TypeEnv.apply final_subst env) trez in
      return (final_subst, trez, EffSet.union erez (EffSet.union eff1 eff2))
    | ETuple es ->
      let* subs, ts, eff =
        RList.fold_left
          es
          ~init:(return (Subst.empty Normal, [], EffSet.empty))
          ~f:(fun (subs, ts, eff) e ->
            let* subs2, t, eff2 = helper env e in
            let* final_subst = Subst.compose subs subs2 in
            return (final_subst, t :: ts, EffSet.union eff eff2))
      in
      return (subs, TTuple ts, eff)
    | ELet ({ is_rec = false; name = x; expr = e1 }, e2) ->
      let* s1, t1, eff1 = helper env e1 in
      let env2 = TypeEnv.apply s1 env in
      let t2 = generalize env2 t1 in
      let* s2, t3, eff2 = helper (TypeEnv.extend env2 (x, t2)) e2 in
      let* final_subst = Subst.compose s1 s2 in
      return (final_subst, t3, EffSet.union eff1 eff2)
    | ELet ({ is_rec = true; name = x; expr = e1 }, e2) ->
      let* tv = fresh_var Normal in
      let env = TypeEnv.extend env (x, S (VarSet.empty, VarSet.empty, tv)) in
      let* s1, t1, eff1 = helper env e1 in
      let* s2 = unify (Subst.apply s1 tv) t1 in
      let* s = Subst.compose s2 s1 in
      let env = TypeEnv.apply s env in
      let t2 = generalize env (Subst.apply s tv) in
      let* s2, t2, eff2 = helper TypeEnv.(extend (apply s env) (x, t2)) e2 in
      let* final_s = Subst.compose s s2 in
      return (final_s, t2, EffSet.union eff1 eff2)
    | EMatch (scr, cases) ->
      let* s, scr_t, scr_eff = helper env scr in
      let* t = fresh_var Normal in
      RList.fold_left
        cases
        ~init:(return (s, t, scr_eff))
        ~f:(fun (s, t, eff) (p, e) ->
          let* p_env, p_t = infer_ptrn p in
          let env =
            List.fold_left p_env ~init:env ~f:(fun env (k, v) ->
                List.Assoc.add env ~equal:String.equal k v)
          in
          let* s2 = unify scr_t p_t in
          let* s3, e_t, eff2 = helper env e in
          let* s4 = unify t e_t in
          let* final_subst = Subst.compose_all [ s; s2; s3; s4 ] in
          return (final_subst, Subst.apply final_subst t, EffSet.union eff eff2))
    | EFun (x, e1) ->
      let* tv = fresh_var Normal in
      let env2 = TypeEnv.extend env (x, S (VarSet.empty, VarSet.empty, tv)) in
      let* s, ty, eff = helper env2 e1 in
      let* ev = fresh_var_eff Normal in
      let trez = TFun (Subst.apply s tv, EffSet.add ev eff, ty) in
      let* trez = unweaken (TypeEnv.apply s env) trez in
      return (s, trez, EffSet.empty)
    | ETry (scr, cases) ->
      let* s, t, scr_eff = helper env scr in
      RList.fold_left
        cases
        ~init:
          (return
             ( s
             , t
             , EffSet.diff
                 scr_eff
                 (EffSet.of_list (List.map cases ~f:(fun (exc, _) -> EffExc exc))) ))
        ~f:(fun (s, t, eff) (_, e) ->
          let* s2, e_t, eff2 = helper env e in
          let* s3 = unify t e_t in
          let* final_subst = Subst.compose_all [ s; s2; s3 ] in
          return (final_subst, Subst.apply final_subst t, EffSet.union eff eff2)))
    >>= fun (s, t, e) -> return (s, Subst.apply s t, Subst.apply_eff s e)
  in
  helper
;;

let standardize_type t =
  let s =
    ( Normal
    , []
    , Caml.List.of_seq
        (Seq.filter_map
           (function
             | b, 1 -> Some (b, EffSet.empty)
             | _ -> None)
           (IntMap.to_seq (Type.free_vars_eff_counted Normal t))) )
  in
  let t = Subst.apply s t in
  let s =
    ( Normal
    , []
    , List.map
        ~f:(fun b -> b, EffSet.empty)
        (Caml.List.of_seq
           (VarSet.to_seq
              (VarSet.diff
                 (Type.free_vars_eff Normal t)
                 (Type.useful_free_vars_eff Normal t)))) )
  in
  let t = Subst.apply s t in
  let s =
    ( Normal
    , List.mapi
        (Caml.List.of_seq (VarSet.to_seq (Type.free_vars Normal t)))
        ~f:(fun i b -> b, TVar (Normal, i))
    , List.mapi
        (Caml.List.of_seq (VarSet.to_seq (Type.free_vars_eff Normal t)))
        ~f:(fun i b -> b, EffSet.singleton (EffVar (Normal, i))) )
  in
  Subst.apply s t
;;

let standardize_program p =
  let* ws = get_weak_s in
  let p = List.map p ~f:(fun (decl, t) -> decl, standardize_type (Subst.apply ws t)) in
  let wts =
    List.fold_left
      ~init:VarSet.empty
      ~f:(fun acc (_, t) -> VarSet.union acc (Type.free_vars Weak t))
      p
  in
  let wts_eff =
    List.fold_left
      ~init:VarSet.empty
      ~f:(fun acc (_, t) -> VarSet.union acc (Type.free_vars_eff Weak t))
      p
  in
  let s =
    ( Weak
    , List.mapi (Caml.List.of_seq (VarSet.to_seq wts)) ~f:(fun i b -> b, TVar (Weak, i))
    , List.mapi
        (Caml.List.of_seq (VarSet.to_seq wts_eff))
        ~f:(fun i b -> b, EffSet.singleton (EffVar (Weak, i))) )
  in
  return (List.map p ~f:(fun (decl, t) -> decl, Subst.apply s t))
;;

module BindSet = Caml.Set.Make (String)

let rec remove_ptrn_vals vals = function
  | PVal name -> BindSet.remove name vals
  | PConst _ -> vals
  | PTuple l -> List.fold_left ~f:remove_ptrn_vals ~init:vals l
  | PCons (ps, p) -> List.fold_left ~f:remove_ptrn_vals ~init:vals (p :: ps)
;;

let rec check_invalid_rec_decl invalid_vals decl =
  check_invalid_rec_expr
    (if decl.is_rec then BindSet.add decl.name invalid_vals else invalid_vals)
    decl.expr

and check_invalid_rec_expr invalid_vals = function
  | EVal name when BindSet.mem name invalid_vals -> fail (IllegalRec name)
  | EConst _ | EVal _ -> return ()
  | EApp (expr1, expr2) ->
    let* () = check_invalid_rec_expr invalid_vals expr1 in
    check_invalid_rec_expr invalid_vals expr2
  | ETuple l -> RList.iter l ~f:(check_invalid_rec_expr invalid_vals)
  | ELet (decl, expr) ->
    let* () = check_invalid_rec_decl invalid_vals decl in
    check_invalid_rec_expr (BindSet.remove decl.name invalid_vals) expr
  | EMatch (scr, cases) ->
    let* () = check_invalid_rec_expr invalid_vals scr in
    RList.iter
      ~f:(fun (ptrn, expr) ->
        check_invalid_rec_expr (remove_ptrn_vals invalid_vals ptrn) expr)
      cases
  | EFun (_, expr) -> check_invalid_rec_expr BindSet.empty expr
  | ETry (scr, cases) ->
    let* () = check_invalid_rec_expr invalid_vals scr in
    RList.iter ~f:(fun (_, expr) -> check_invalid_rec_expr invalid_vals expr) cases
;;

let check_invalid_rec_program = RList.iter ~f:(check_invalid_rec_decl BindSet.empty)

let infer_program p =
  run
    (let* () = check_invalid_rec_program p in
     let* _, p =
       RList.fold_left
         p
         ~init:(return (TypeEnv.empty, []))
         ~f:(fun (env, acc) decl ->
           let* _, t, _ = infer env (ELet (decl, EVal decl.name)) in
           return (TypeEnv.extend env (decl.name, generalize env t), (decl, t) :: acc))
     in
     let* p = standardize_program p in
     return (List.rev p))
;;
