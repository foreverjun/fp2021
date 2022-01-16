open Ast
open Format

type var_kind =
  | Normal
  | Weak
[@@deriving eq, ord]

let pp_var_kind fmt = function
  | Normal -> fprintf fmt "'"
  | Weak -> fprintf fmt "'_weak_"
;;

type eff =
  | EffIO
  | EffAsgmt
  | EffExc of exc
  | EffVar of var_kind * int
[@@deriving eq, ord]

let eff_io = EffIO
let eff_asgmt = EffAsgmt
let eff_exc exc = EffExc exc
let eff_var kind b = EffVar (kind, b)
let all_effs = [ eff_exc exc1; eff_exc exc2; eff_io; eff_asgmt ]

let pp_eff fmt = function
  | EffIO -> fprintf fmt "IO"
  | EffAsgmt -> fprintf fmt "Asgmt"
  | EffExc exc -> fprintf fmt "exc %a" pp_exc exc
  | EffVar (kind, b) -> fprintf fmt "%ae%d" pp_var_kind kind b
;;

module EffSet = Set.Make (struct
  let compare = compare_eff

  type t = eff
end)

type eff_set = EffSet.t

let pp_eff_set fmt effs =
  if EffSet.is_empty effs
  then fprintf fmt "-->"
  else
    fprintf
      fmt
      "-[%a]->"
      (pp_print_seq
         ~pp_sep:(fun fmt () -> fprintf fmt ", ")
         (fun fmt eff -> pp_eff fmt eff))
      (EffSet.to_seq effs)
;;

let equal_eff_set effs1 effs2 = EffSet.subset effs1 effs2 && EffSet.subset effs2 effs1

type binder = int [@@deriving show { with_path = false }]

module VarSet = struct
  include Caml.Set.Make (Int)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
  ;;
end

type binder_set = VarSet.t [@@deriving show { with_path = false }]

type ty =
  | TInt
  | TString
  | TBool
  | TExc of exc
  | TTuple of ty list
  | TList of ty
  | TRef of ty
  | TVar of var_kind * int
  | TFun of ty * eff_set * ty
[@@deriving eq]

let t_int = TInt
let t_string = TString
let t_bool = TBool
let t_exc exc = TExc exc
let t_tuple tys = TTuple tys
let t_unit = t_tuple []
let t_list ty = TList ty
let t_ref ty = TRef ty
let t_var kind b = TVar (kind, b)
let t_fun arg_ty effs ret_ty = TFun (arg_ty, effs, ret_ty)

let rec pp_ty fmt = function
  | TInt -> fprintf fmt "int"
  | TString -> fprintf fmt "string"
  | TBool -> fprintf fmt "bool"
  | TExc exc -> pp_exc fmt exc
  | TTuple [] -> fprintf fmt "unit"
  | TTuple tys ->
    fprintf
      fmt
      "(%a)"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> fprintf fmt " * ")
         (fun fmt ty -> pp_ty fmt ty))
      (List.rev tys)
  | TList ty -> fprintf fmt "%a list" pp_ty ty
  | TRef ty -> fprintf fmt "%a ref" pp_ty ty
  | TVar (kind, b) -> fprintf fmt "%at%d" pp_var_kind kind b
  | TFun (arg_ty, effs, ret_ty) ->
    fprintf fmt "(%a %a %a)" pp_ty arg_ty pp_eff_set effs pp_ty ret_ty
;;

type scheme = S of binder_set * binder_set * ty

let pp_scheme ppf (S (xs, ys, t)) =
  if VarSet.is_empty xs && VarSet.is_empty ys
  then pp_ty ppf t
  else fprintf ppf "forall %a; %a . %a" VarSet.pp xs VarSet.pp ys pp_ty t
;;

type subst = var_kind * (int * ty) list * (int * eff_set) list
