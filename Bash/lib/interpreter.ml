open Ast

(* -------------------- Monads to be passed to the interpreter -------------------- *)

(** Infix monad with a fail function *)
module type MonadFail = sig
  include Base.Monad.Infix

  val return : 'a -> 'a t
  val fail : string -> 'a t
end

(** Result as a monad-fail *)
module Result : MonadFail with type 'a t = ('a, string) result = struct
  type 'a t = ('a, string) result

  let return = Result.ok
  let ( >>= ) = Result.bind
  let ( >>| ) f g = f >>= fun x -> return (g x)
  let fail = Result.error
end

(* -------------------- Maps with pretty-printing -------------------- *)

(** Ordered type with pretty-printing *)
module type PpOrderedType = sig
  include Map.OrderedType

  val pp_t : Format.formatter -> t -> unit
end

(** Functor to create maps with pretty-printing for deriving *)
module TMap (T : PpOrderedType) = struct
  include Map.Make (T)

  let pp pp_v ppf m =
    Format.fprintf ppf "@[[@[";
    iter (fun k v -> Format.fprintf ppf "@[%a: %a@],@\n" T.pp_t k pp_v v) m;
    Format.fprintf ppf "@]]@]"
  ;;
end

(** Map with string keys *)
module SMap = TMap (struct
  include String

  let pp_t = Format.pp_print_string
end)

(** Map with integer keys *)
module IMap = TMap (struct
  include Int

  let pp_t = Format.pp_print_int
end)

(* -------------------- Interpreter -------------------- *)

(** Main interpreter module *)
module Eval (M : MonadFail) = struct
  open M

  (* -------------------- Environment -------------------- *)

  (** Container for values of variables *)
  type var_t =
    | Val of string (** Simple variable *)
    | IndArray of string IMap.t (** Indexed array *)
    | AssocArray of string SMap.t (** Associative array *)
  [@@deriving show { with_path = false }]

  (** Container for functions *)
  type fun_t = compound [@@deriving show { with_path = false }]

  type variables = var_t SMap.t [@@deriving show { with_path = false }]
  type functions = fun_t SMap.t [@@deriving show { with_path = false }]

  (** Complete environment *)
  type environment =
    { vars : variables
    ; funs : functions
    ; retcode : int
    }
  [@@deriving show { with_path = false }]

  let get_var name env = SMap.find_opt name env.vars
  let set_var name v env = SMap.add name v env.vars
  let get_fun name env = SMap.find_opt name env.funs
  let set_fun name v env = SMap.add name v env.funs

  (* -------------------- Evaluation -------------------- *)

  (** Evaluate variable *)
  let ev_var env =
    let find f a i =
      match f i a with
      | None -> ""
      | Some v -> v
    in
    function
    | SimpleVar name ->
      (match get_var name env with
      | None -> return ""
      | Some (Val v) -> return v
      | Some (IndArray vs) -> return (find IMap.find_opt vs 0)
      | Some (AssocArray vs) -> return (find SMap.find_opt vs "0"))
    | Subscript (name, index) ->
      (match get_var name env with
      | None -> return ""
      | Some (Val v) ->
        (match int_of_string_opt index with
        | None -> return v
        | Some i when i = 0 -> return v
        | Some _ -> return "")
      | Some (IndArray vs) ->
        (match int_of_string_opt index with
        | None -> return (find IMap.find_opt vs 0)
        | Some i -> return (find IMap.find_opt vs i))
      | Some (AssocArray vs) -> return (find SMap.find_opt vs index))
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

  (** Evaluate parameter expansion *)
  let ev_param_exp env =
    let subst size ~all ~beg v p r =
      ev_var env v
      >>| fun s ->
      if s = "" && Base.String.for_all s ~f:(fun c -> c = '*')
      then r (* A hack because Re.replace does nothing on an empty string *)
      else (
        let cond g =
          let c =
            match beg with
            | None -> true (* Anywhere *)
            | Some true -> Re.Group.start g 0 = 0 (* Only at the beginning *)
            | Some false -> Re.Group.stop g 0 = String.length s
            (* Only at the end *)
          in
          if c then r else Re.Group.get g 0
        in
        let re = Re.Glob.glob p |> size |> Re.compile in
        Re.replace ~all:(all || beg = Some false) re ~f:cond s)
      (* ~all has to be true when matching only at the end *)
    in
    function
    | Param v -> ev_var env v
    | PosParam i -> ev_var env (SimpleVar (string_of_int i))
    | Length v -> ev_var env v >>| fun s -> string_of_int (String.length s)
    | Substring (v, pos, len) ->
      ev_var env v
      >>= fun s ->
      ev_arithm env pos
      >>= fun pos ->
      (match len with
      | Some a -> ev_arithm env a >>| fun n -> Some n
      | None -> return None)
      >>= fun len ->
      let s_len = String.length s in
      let p = if 0 <= pos && pos < s_len then pos else s_len + pos in
      let l =
        match len with
        | Some l when l >= 0 -> min l (s_len - p)
        | Some l -> s_len + l - p
        | None -> s_len - p
      in
      if p >= 0 && l >= 0
      then return (String.sub s p l)
      else if l >= 0
      then return ""
      else fail "substring expression < 0"
    | CutMinBeg (v, p) -> subst Re.shortest ~all:false ~beg:(Some true) v p ""
    | CutMaxBeg (v, p) -> subst Re.longest ~all:false ~beg:(Some true) v p ""
    | CutMinEnd (v, p) -> subst Re.shortest ~all:false ~beg:(Some false) v p ""
    | CutMaxEnd (v, p) -> subst Re.longest ~all:false ~beg:(Some false) v p ""
    | SubstOne (v, p, r) -> subst Re.longest ~all:false ~beg:None v p r
    | SubstAll (v, p, r) -> subst Re.longest ~all:true ~beg:None v p r
    | SubstBeg (v, p, r) -> subst Re.longest ~all:false ~beg:(Some true) v p r
    | SubstEnd (v, p, r) -> subst Re.longest ~all:false ~beg:(Some false) v p r
  ;;

  (** Evaluate filename expansion *)
  let ev_filename_exp _ s =
    let cts dir =
      let re = Re.Glob.glob s |> Re.whole_string |> Re.compile in
      let rd d = Array.to_list (Sys.readdir d) in
      (* Source: https://gist.github.com/lindig/be55f453026c65e761f4e7012f8ab9b5 *)
      let rec helper res = function
        | d :: tl when Sys.is_directory d ->
          rd d
          |> List.map (Filename.concat d)
          |> List.append tl
          |> if Re.execp re d then helper (d :: res) else helper res
        | f :: tl when Re.execp re f -> helper (f :: res) tl
        | _ :: tl -> helper res tl
        | [] -> res
      in
      helper [] (rd dir)
    in
    cts (Sys.getcwd ()) |> List.sort String.compare |> return
  ;;

  (** Evaluate word *)
  let rec ev_word (env : environment) : word -> string list t = function
    | BraceExp ws ->
      let rec helper res = function
        | hd :: tl -> ev_word env hd >>= fun rs -> helper (rs @ res) tl
        | [] -> return res
      in
      helper [] (List.rev ws)
    | ParamExp p -> ev_param_exp env p >>| fun s -> [ s ]
    | CmdSubst _ -> fail "Not implemented"
    | ArithmExp a -> ev_arithm env a >>| fun n -> [ string_of_int n ]
    | FilenameExp s -> ev_filename_exp env s
    | Word s -> return [ s ]
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
  | _ -> "Not implemented"
;;

(* ----------------------------------------------- *)
(* -------------------- Tests -------------------- *)
(* ----------------------------------------------- *)

open Eval (Result)

(* -------------------- Helper functions -------------------- *)

let empty_env = { vars = SMap.empty; funs = SMap.empty; retcode = 0 }

let succ_ev ?(env = empty_env) pp_giv pp_res ev giv exp =
  match ev env giv with
  | Error e ->
    Printf.printf "Error: %s\n" e;
    false
  | Ok res when exp = res -> true
  | Ok res ->
    print_string "\n-------------------- Input --------------------\n";
    pp_giv Format.std_formatter giv;
    Format.pp_print_flush Format.std_formatter ();
    print_string "\n----------------- Environment --------------------\n";
    pp_environment Format.std_formatter env;
    Format.pp_print_flush Format.std_formatter ();
    print_string "\n------------------- Expected ------------------\n";
    pp_res Format.std_formatter exp;
    Format.pp_print_flush Format.std_formatter ();
    print_string "\n-------------------- Actual -------------------\n";
    pp_res Format.std_formatter res;
    Format.pp_print_flush Format.std_formatter ();
    print_string "\n-----------------------------------------------\n";
    flush stdout;
    false
;;

let fail_ev ?(env = empty_env) pp_giv pp_res ev giv exp =
  match ev env giv with
  | Error e when exp = e -> true
  | Error e ->
    print_string "\n-------------------- Input --------------------\n";
    pp_giv Format.std_formatter giv;
    Format.pp_print_flush Format.std_formatter ();
    print_string "\n----------------- Environment --------------------\n";
    pp_environment Format.std_formatter env;
    Format.pp_print_flush Format.std_formatter ();
    print_string "\n--------------- Unexpected error ------------------\n";
    print_string e;
    print_string "\n-----------------------------------------------\n";
    flush stdout;
    false
  | Ok res ->
    print_string "\n-------------------- Input --------------------\n";
    pp_giv Format.std_formatter giv;
    Format.pp_print_flush Format.std_formatter ();
    print_string "\n----------------- Environment --------------------\n";
    pp_environment Format.std_formatter env;
    Format.pp_print_flush Format.std_formatter ();
    print_string "\n-------------------- Actual -------------------\n";
    pp_res Format.std_formatter res;
    Format.pp_print_flush Format.std_formatter ();
    print_string "\n-----------------------------------------------\n";
    flush stdout;
    false
;;

(* -------------------- Variable -------------------- *)

let succ_ev_var ?(env = empty_env) = succ_ev ~env pp_var Format.pp_print_string ev_var

let%test _ = succ_ev_var (SimpleVar "ABC") ""
let%test _ = succ_ev_var (Subscript ("ABC", "0")) ""

let%test _ =
  succ_ev_var
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "2") }
    (SimpleVar "ABC")
    "2"
;;

let%test _ =
  succ_ev_var
    ~env:
      { empty_env with
        vars =
          SMap.singleton
            "ABC"
            (IndArray (IMap.of_seq (List.to_seq [ 0, "a"; 1, "b"; 2, "c" ])))
      }
    (SimpleVar "ABC")
    "a"
;;

let%test _ =
  succ_ev_var
    ~env:
      { empty_env with
        vars =
          SMap.singleton
            "ABC"
            (IndArray (IMap.of_seq (List.to_seq [ 0, "a"; 1, "b"; 2, "c" ])))
      }
    (Subscript ("ABC", "1"))
    "b"
;;

let%test _ =
  succ_ev_var
    ~env:
      { empty_env with
        vars =
          SMap.singleton
            "ABC"
            (IndArray (IMap.of_seq (List.to_seq [ 0, "a"; 1, "b"; 2, "c" ])))
      }
    (Subscript ("ABC", "3"))
    ""
;;

let%test _ =
  succ_ev_var
    ~env:
      { empty_env with
        vars =
          SMap.singleton
            "ABC"
            (AssocArray (SMap.of_seq (List.to_seq [ "a", "a1"; "b", "b1"; "0", "01" ])))
      }
    (SimpleVar "ABC")
    "01"
;;

let%test _ =
  succ_ev_var
    ~env:
      { empty_env with
        vars =
          SMap.singleton
            "ABC"
            (AssocArray (SMap.of_seq (List.to_seq [ "a", "a1"; "b", "b1"; "0", "01" ])))
      }
    (Subscript ("ABC", "b"))
    "b1"
;;

let%test _ =
  succ_ev_var
    ~env:
      { empty_env with
        vars =
          SMap.singleton
            "ABC"
            (AssocArray (SMap.of_seq (List.to_seq [ "a", "a1"; "b", "b1"; "0", "01" ])))
      }
    (Subscript ("ABC", "!"))
    ""
;;

(* -------------------- Arithmetic -------------------- *)

let succ_ev_arithm ?(env = empty_env) =
  succ_ev ~env pp_arithm Format.pp_print_int ev_arithm
;;

let fail_ev_arithm ?(env = empty_env) =
  fail_ev ~env pp_arithm Format.pp_print_int ev_arithm
;;

let%test _ = succ_ev_arithm (Plus (Num 1, Num 2)) 3
let%test _ = succ_ev_arithm (Div (Num 1, Num 3)) 0
let%test _ = succ_ev_arithm (Div (Num 2, Num 3)) 0
let%test _ = succ_ev_arithm (Div (NEqual (Num 1, Num 2), Greater (Num 3, Num 1))) 1
let%test _ = fail_ev_arithm (Div (Num 1, Num 0)) "Division by 0"

(* -------------------- Parameter expansion -------------------- *)

let succ_ev_param_exp ?(env = empty_env) =
  succ_ev ~env pp_param_exp Format.pp_print_string ev_param_exp
;;

let fail_ev_param_exp ?(env = empty_env) =
  fail_ev ~env pp_param_exp Format.pp_print_string ev_param_exp
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "abc") }
    (Param (SimpleVar "ABC"))
    "abc"
;;

let%test _ = succ_ev_param_exp (Param (SimpleVar "ABC")) ""

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "3" (Val "123") }
    (PosParam 3)
    "123"
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "12345") }
    (Length (SimpleVar "ABC"))
    "5"
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "") }
    (Length (SimpleVar "ABC"))
    "0"
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "01234") }
    (Substring (SimpleVar "ABC", Num 0, Some (Num 3)))
    "012"
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "01234") }
    (Substring (SimpleVar "ABC", Num 0, Some (Num 5)))
    "01234"
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "01234") }
    (Substring (SimpleVar "ABC", Num 0, Some (Num 7)))
    "01234"
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "01234") }
    (Substring (SimpleVar "ABC", Num 2, Some (Num 2)))
    "23"
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "01234") }
    (Substring (SimpleVar "ABC", Num 2, Some (Num 0)))
    ""
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "01234") }
    (Substring (SimpleVar "ABC", Num 2, None))
    "234"
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "01234") }
    (Substring (SimpleVar "ABC", Num (-3), Some (Num 2)))
    "23"
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "01234") }
    (Substring (SimpleVar "ABC", Num (-1), Some (Num 2)))
    "4"
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "01234") }
    (Substring (SimpleVar "ABC", Num (-6), Some (Num 3)))
    ""
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "01234") }
    (Substring (SimpleVar "ABC", Num (-3), Some (Num (-2))))
    "2"
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "01234") }
    (Substring (SimpleVar "ABC", Num (-3), Some (Num (-3))))
    ""
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "01234") }
    (Substring (SimpleVar "ABC", Num 0, Some (Num (-5))))
    ""
;;

let%test _ =
  fail_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "01234") }
    (Substring (SimpleVar "ABC", Num 4, Some (Num (-3))))
    "substring expression < 0"
;;

let%test _ =
  fail_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "01234") }
    (Substring (SimpleVar "ABC", Num 0, Some (Num (-6))))
    "substring expression < 0"
;;

let%test _ =
  fail_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "01234") }
    (Substring (SimpleVar "ABC", Div (Num 1, Num 0), None))
    "Division by 0"
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "01234") }
    (CutMinBeg (SimpleVar "ABC", "*"))
    "01234"
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "01234") }
    (CutMinBeg (SimpleVar "ABC", "?"))
    "1234"
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "01234") }
    (CutMinBeg (SimpleVar "ABC", "[0-9][0-9]"))
    "234"
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "01234") }
    (CutMinBeg (SimpleVar "ABC", "23"))
    "01234"
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "01234") }
    (CutMinBeg (SimpleVar "ABC", "01"))
    "234"
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "01234") }
    (CutMaxBeg (SimpleVar "ABC", "*"))
    ""
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "01234") }
    (CutMinEnd (SimpleVar "ABC", "?"))
    "0123"
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "01234") }
    (CutMinEnd (SimpleVar "ABC", "*"))
    "01234"
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "01234") }
    (CutMaxEnd (SimpleVar "ABC", "?"))
    "0123"
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "01234") }
    (CutMaxEnd (SimpleVar "ABC", "*"))
    ""
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "01234") }
    (SubstOne (SimpleVar "ABC", "?", "a"))
    "a1234"
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "01234") }
    (SubstOne (SimpleVar "ABC", "*", "a"))
    "a"
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "") }
    (SubstOne (SimpleVar "ABC", "*", "a"))
    "a"
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "01234") }
    (SubstAll (SimpleVar "ABC", "?", "a"))
    "aaaaa"
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "01234") }
    (SubstAll (SimpleVar "ABC", "*", "a"))
    "a"
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "abacab") }
    (SubstAll (SimpleVar "ABC", "a", "heh"))
    "hehbhehchehb"
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "01234") }
    (SubstBeg (SimpleVar "ABC", "*", "a"))
    "a"
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "abcabab") }
    (SubstBeg (SimpleVar "ABC", "ab", "!"))
    "!cabab"
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "01234") }
    (SubstEnd (SimpleVar "ABC", "*", "a"))
    "a"
;;

let%test _ =
  succ_ev_param_exp
    ~env:{ empty_env with vars = SMap.singleton "ABC" (Val "abcabab") }
    (SubstEnd (SimpleVar "ABC", "ab", "!"))
    "abcab!"
;;

(* -------------------- Filename expansion -------------------- *)

let succ_ev_filename_exp ?(env = empty_env) =
  succ_ev
    ~env
    Format.pp_print_string
    (Format.pp_print_list ~pp_sep:Format.pp_print_newline Format.pp_print_string)
    ev_filename_exp
;;

let cwd_satisfy c =
  let cwd = Sys.getcwd () in
  cwd
  |> Sys.readdir
  |> Array.to_list
  |> List.filter (fun f ->
         Sys.file_exists (Filename.concat cwd f) && (not (f.[0] = '.')) && c f)
  |> List.sort String.compare
;;

let%test _ =
  succ_ev_filename_exp "*.ml" (cwd_satisfy (fun f -> Filename.check_suffix f ".ml"))
;;

(* -------------------- Word -------------------- *)

let succ_ev_word ?(env = empty_env) =
  succ_ev
    ~env
    pp_word
    (Format.pp_print_list ~pp_sep:Format.pp_print_newline Format.pp_print_string)
    ev_word
;;

let fail_ev_word ?(env = empty_env) =
  fail_ev
    ~env
    pp_word
    (Format.pp_print_list ~pp_sep:Format.pp_print_newline Format.pp_print_string)
    ev_word
;;

let%test _ =
  succ_ev_word (BraceExp [ ParamExp (Param (SimpleVar "X")); Word "y" ]) [ ""; "y" ]
;;

let%test _ =
  succ_ev_word
    ~env:{ empty_env with vars = SMap.singleton "M" (Val "meow") }
    (ParamExp (Param (SimpleVar "M")))
    [ "meow" ]
;;

let%test _ = succ_ev_word (ArithmExp (Num 5)) [ "5" ]
let%test _ = fail_ev_word (ArithmExp (Div (Num 5, Num 0))) "Division by 0"

let%test _ =
  succ_ev_word (FilenameExp "*.*") (cwd_satisfy (fun f -> String.contains f '.'))
;;

let%test _ = succ_ev_word (Word "hey") [ "hey" ]
