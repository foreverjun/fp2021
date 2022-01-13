(** Infix monad with a fail function *)
module type MonadFail = sig
  include Base.Monad.Infix

  val return : 'a -> 'a t
  val fail : string -> 'a t
  val ( <|> ) : 'a t -> (unit -> 'a t) -> 'a t
end

(** Result as a monad-fail *)
module Result : MonadFail with type 'a t = ('a, string) result

module IMap : Map.S with type key = int
module SMap : Map.S with type key = string

module Eval (M : MonadFail) : sig
  (** Container for values of variables *)
  type var_t =
    | IndArray of string IMap.t (** Simple variable or indexed array *)
    | AssocArray of string SMap.t (** Associative array *)

  (** Complete environment *)
  type environment =
    { vars : var_t SMap.t (** Variables available in the current scope *)
    ; funs : (Ast.compound * Ast.redir list) SMap.t
          (** Functions available in the current scope *)
    ; chs : Unix.file_descr IMap.t (** IO channels file descriptors *)
    ; retcode : int (** Return code of the last operation *)
    }

  val empty_env : environment

  (** Evaluate Bash script *)
  val ev_script : environment -> Ast.script -> environment M.t
end
