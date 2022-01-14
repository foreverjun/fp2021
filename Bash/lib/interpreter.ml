open Ast

(* -------------------- Monads to be passed to the interpreter -------------------- *)

(** Infix monad with a fail function *)
module type MonadFail = sig
  include Base.Monad.Infix

  val return : 'a -> 'a t
  val fail : string -> 'a t
  val ( <|> ) : 'a t -> (unit -> 'a t) -> 'a t
end

(** Result as a monad-fail *)
module Result : MonadFail with type 'a t = ('a, string) result = struct
  type 'a t = ('a, string) result

  let return = Result.ok
  let ( >>= ) = Result.bind
  let ( >>| ) f g = f >>= fun x -> return (g x)

  let ( <|> ) f g =
    match f with
    | Ok _ -> f
    | Error _ -> g ()
  ;;

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

  let of_list l = of_seq (List.to_seq l)
  let add_list l = add_seq (List.to_seq l)

  let pp pp_v ppf m =
    let open Format in
    fprintf ppf "@[[@[";
    iter (fun k v -> fprintf ppf "@[%a: %a@],@\n" T.pp_t k pp_v v) m;
    fprintf ppf "@]]@]"
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

(* -------------------- Helper functions -------------------- *)

(** [read_all fd] reads all contents of the file descriptor [fd] until EOF is reached *)
let read_all fd = Stdio.In_channel.input_all (Unix.in_channel_of_descr fd)

(* -------------------- Standard library -------------------- *)

(** Built-in functions *)
module Builtins : sig
  (** BuiltIn functions in the form of [cla -> chs -> retcode] *)
  type t = string list -> Unix.file_descr IMap.t -> int

  (** [print_err chs s] prints [s] to stderr from [chs] if it exists *)
  val print_err : Unix.file_descr IMap.t -> string -> unit

  (** [find s] returns builtin with the name [s] if such builtin exists *)
  val find : string -> t option
end = struct
  type t = string list -> Unix.file_descr IMap.t -> int

  let try_read fd =
    try read_all fd with
    | Unix.Unix_error (Unix.EBADF, "read", "") -> ""
  ;;

  let try_wr fd s =
    let len = String.length s in
    try Unix.write_substring fd s 0 len = len with
    | Unix.Unix_error (Unix.EBADF, "write", "") -> false
  ;;

  let print_err chs s =
    match IMap.find_opt 2 chs with
    | Some stderr when try_wr stderr (s ^ "\n") -> ()
    | _ -> ()
  ;;

  let echo argv chs =
    match IMap.find_opt 1 chs, argv with
    | Some stdout, _ :: args
      when try_wr stdout (String.concat " " (List.filter (( <> ) "") args) ^ "\n") -> 0
    | Some _, _ :: _ ->
      print_err chs "echo (miniBash): failed to write to stdout";
      1
    | None, _ ->
      print_err chs "echo (miniBash): stdout is absent";
      1
    | Some _, [] ->
      print_err chs "echo (miniBash): no arguments (not even a command name)";
      1
  ;;

  let find : string -> t option = function
    | "echo" -> Some echo
    | _ -> None
  ;;
end

(* -------------------- Interpreter -------------------- *)

(** Main interpreter module *)
module Eval (M : MonadFail) = struct
  open M

  (* -------------------- Environment -------------------- *)

  (** Container for values of variables *)
  type var_t =
    | IndArray of string IMap.t (** Simple variable or indexed array *)
    | AssocArray of string SMap.t (** Associative array *)
  [@@deriving show { with_path = false }]

  (** Complete environment *)
  type environment =
    { vars : var_t SMap.t (** Variables available in the current scope *)
    ; funs : (compound * redir list) SMap.t
          (** Functions available in the current scope *)
    ; chs : Unix.file_descr IMap.t
          [@printer fun fmt m -> IMap.pp (fun fmt _ -> fprintf fmt "[...]") fmt m]
          (** IO channels file descriptors *)
    ; retcode : int (** Return code of the last operation *)
    }
  [@@deriving show { with_path = false }]

  let empty_env =
    { vars = SMap.empty
    ; funs = SMap.empty
    ; chs = Unix.(IMap.of_list [ 0, stdin; 1, stdout; 2, stderr ])
    ; retcode = 0
    }
  ;;

  let get_var name env = SMap.find_opt name env.vars
  let set_var name v env = { env with vars = SMap.add name v env.vars }
  let get_fun name env = SMap.find_opt name env.funs
  let set_fun name v env = { env with funs = SMap.add name v env.funs }

  let merge_envs e1 e2 =
    { e2 with
      vars = SMap.union (fun _ _ v -> Some v) e1.vars e2.vars
    ; funs = SMap.union (fun _ _ v -> Some v) e1.funs e2.funs
    ; chs = IMap.union (fun _ _ v -> Some v) e1.chs e2.chs
    }
  ;;

  (* -------------------- Evaluation -------------------- *)

  (** Evaluate variable *)
  let ev_var env ((name, index) : var) =
    let find f a i =
      match f i a with
      | None -> ""
      | Some v -> v
    in
    match get_var name env with
    | None -> return ""
    | Some (IndArray vs) ->
      (match int_of_string_opt index with
      | None -> return (find IMap.find_opt vs 0)
      | Some i -> return (find IMap.find_opt vs i))
    | Some (AssocArray vs) -> return (find SMap.find_opt vs index)
  ;;

  (** Evaluate filename expansion *)
  let ev_filename_exp _ s =
    let cts dir =
      let re =
        Re.(
          Glob.(
            try glob ~anchored:true s with
            | Parse_error -> empty)
          |> compile)
      in
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
    cts (Sys.getcwd ())
    |> List.sort String.compare
    |> function
    | _ :: _ as ss -> return ss
    | [] -> return [ s ]
  ;;

  (** Evaluate word *)
  let rec ev_word env = function
    | DoubleQuotes ws -> ev_words env ws >>| fun (env, ss) -> env, [ String.concat "" ss ]
    | BraceExp ws -> ev_words env ws
    | ParamExp p -> ev_param_exp env p >>| fun (env, s) -> env, [ s ]
    | CmdSubst c ->
      let rd, wr = Unix.pipe () in
      ev_cmd { env with chs = IMap.add 1 wr env.chs } c
      >>| fun { retcode } ->
      Unix.close wr;
      let s = read_all rd in
      Unix.close rd;
      { env with retcode }, [ Base.String.rstrip s ]
    | ArithmExp a -> ev_arithm env a >>| fun (env, n) -> env, [ string_of_int n ]
    | FilenameExp s -> ev_filename_exp env s >>| fun ss -> env, ss
    | Word s -> return (env, [ s ])

  (** Evaluate word list *)
  and ev_words env ?(c = fun _ -> true) ?(e = "Evaluated words condition mismatch") ws =
    let ( @+ ) old_env { chs; retcode } = { old_env with chs; retcode } in
    List.fold_left
      (fun acc w ->
        acc
        >>= fun (env, l) ->
        ev_word env w
        >>= fun (n_env, ss) -> if c ss then return (env @+ n_env, ss :: l) else fail e)
      (return (env, []))
      ws
    >>| fun (env, l) -> env, List.(l |> rev |> flatten)

  (** Evaluate assignment *)
  and ev_assignt env ?(s_env = env) =
    let open struct
      type 'a container =
        | Simple of string * 'a (** Simple assignment of key and value *)
        | Ind of 'a list (** Indexed array assignment of values *)
        | Assoc of (string * 'a) list (** Associative array assignment of pairs *)
    end in
    let env_set env name =
      let env_with x = { (set_var name x env) with retcode = 0 } in
      function
      | Simple (k, v) ->
        env_with
          (match get_var name env, int_of_string_opt k with
          | None, None -> AssocArray (SMap.singleton k v)
          | None, Some i -> IndArray (IMap.singleton i v)
          | Some (IndArray a), None -> IndArray (IMap.add 0 v a)
          | Some (IndArray a), Some i -> IndArray (IMap.add i v a)
          | Some (AssocArray a), _ -> AssocArray (SMap.add k v a))
      | Ind vs -> env_with (IndArray (vs |> List.mapi (fun i v -> i, v) |> IMap.of_list))
      | Assoc ps -> env_with (AssocArray (SMap.of_list ps))
    in
    function
    | SimpleAssignt ((name, i), w) ->
      ev_word s_env w
      >>= (function
      | _, [ s ] -> return (env_set env name (Simple (i, s)))
      | _ -> fail "Illegal expansion in simple assignment")
    | IndArrAssignt (name, ws) ->
      ev_words s_env ws >>| fun (_, ss) -> env_set env name (Ind ss)
    | AssocArrAssignt (name, ps) ->
      ev_words
        s_env
        ~c:(function
          | [ _ ] -> true
          | _ -> false)
        ~e:"Illegal expansion in associative array assignment"
        (List.map (fun (_, w) -> w) ps)
      >>| fun (_, ss) -> env_set env name (Assoc (List.map2 (fun (k, _) v -> k, v) ps ss))

  (** Evaluate asiignment list *)
  and ev_assignts env ?(s_env = env) assignts =
    List.fold_left
      (fun acc a ->
        acc
        >>= fun (env, s_env) ->
        ev_assignt env ~s_env a
        >>= fun env -> ev_assignt s_env a >>| fun s_env -> env, s_env)
      (return (env, s_env))
      assignts
    >>| fun (env, _) -> env

  (** Evaluate bare arithmetic *)
  and ev_arithm env =
    let rec ev_ari ?(c = fun _ _ -> true) ?(e = "") env op l r =
      ev env l
      >>= fun (env, l) ->
      ev env r >>= fun (env, r) -> if c l r then return (env, op l r) else fail e
    and ev_log env op l r =
      ev env l
      >>= fun (env, l) -> ev env r >>| fun (env, r) -> if op l r then env, 1 else env, 0
    and ev env = function
      | Num n -> return (env, n)
      | Var x ->
        ev_var env x
        >>| fun s ->
        (match int_of_string_opt s with
        | None -> env, 0
        | Some n -> env, n)
      | ArithmAssignt (x, v) ->
        ev_arithm env v
        >>= fun (env, n) ->
        ev_assignt env (SimpleAssignt (x, Word (string_of_int n))) >>| fun env -> env, n
      | Plus (l, r) -> ev_ari env ( + ) l r
      | Minus (l, r) -> ev_ari env ( - ) l r
      | Mul (l, r) -> ev_ari env ( * ) l r
      | Div (l, r) -> ev_ari ~c:(fun _ r -> r <> 0) ~e:"Division by 0" env ( / ) l r
      | Less (l, r) -> ev_log env ( < ) l r
      | Greater (l, r) -> ev_log env ( > ) l r
      | LessEq (l, r) -> ev_log env ( <= ) l r
      | GreaterEq (l, r) -> ev_log env ( >= ) l r
      | Equal (l, r) -> ev_log env ( = ) l r
      | NEqual (l, r) -> ev_log env ( <> ) l r
    in
    ev env

  (** Evaluate parameter expansion *)
  and ev_param_exp env =
    let subst size ~all ~beg v p r =
      ev_var env v
      >>| fun s ->
      if s = "" && Base.String.for_all s ~f:(fun c -> c = '*')
      then env, r (* A hack because Re.replace does nothing on an empty string *)
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
        env, Re.replace ~all:(all || beg = Some false) re ~f:cond s)
      (* ~all has to be true when matching only at the end *)
    in
    function
    | Param v -> ev_var env v >>| fun s -> env, s
    | Length v -> ev_var env v >>| fun s -> env, string_of_int (String.length s)
    | Substring (v, pos, len) ->
      ev_var env v
      >>= fun s ->
      ev_arithm env pos
      >>= fun (env, pos) ->
      (match len with
      | Some a -> ev_arithm env a >>| fun (env, n) -> env, Some n
      | None -> return (env, None))
      >>= fun (env, len) ->
      let s_len = String.length s in
      let p = if 0 <= pos && pos < s_len then pos else s_len + pos in
      let l =
        match len with
        | Some l when l >= 0 -> min l (s_len - p)
        | Some l -> s_len + l - p
        | None -> s_len - p
      in
      if p >= 0 && l >= 0
      then return (env, String.sub s p l)
      else if l >= 0
      then return (env, "")
      else fail "substring expression < 0"
    | CutMinBeg (v, p) -> subst Re.shortest ~all:false ~beg:(Some true) v p ""
    | CutMaxBeg (v, p) -> subst Re.longest ~all:false ~beg:(Some true) v p ""
    | CutMinEnd (v, p) -> subst Re.shortest ~all:false ~beg:(Some false) v p ""
    | CutMaxEnd (v, p) -> subst Re.longest ~all:false ~beg:(Some false) v p ""
    | SubstOne (v, p, r) -> subst Re.longest ~all:false ~beg:None v p r
    | SubstAll (v, p, r) -> subst Re.longest ~all:true ~beg:None v p r
    | SubstBeg (v, p, r) -> subst Re.longest ~all:false ~beg:(Some true) v p r
    | SubstEnd (v, p, r) -> subst Re.longest ~all:false ~beg:(Some false) v p r

  (** Evaluate redirection (when duplicationg, no checks are performed for r-w access) *)
  and ev_redir env ?(s_env = env) =
    (* Permissions for new files: rw for user, r for group, none for others *)
    let perm = 0o640 in
    let to_str env w =
      ev_word env w
      >>= fun (_, ss) ->
      match ss with
      | [ s ] -> return s
      | _ -> fail "Illegal expansion in redirection"
    in
    let env_add_fd env n fd = return { env with chs = IMap.add n fd env.chs } in
    let env_dup_fd env n s =
      match int_of_string_opt s with
      | Some i ->
        (match IMap.find_opt i env.chs with
        | Some fd -> env_add_fd env n fd
        | None -> fail (Printf.sprintf "%i: Bad file descriptor" i))
      | None -> fail (Printf.sprintf "%s: ambiguous redirect" s)
    in
    function
    | RedirInp (n, w) ->
      to_str s_env w
      >>= fun f ->
      if Sys.file_exists f
      then env_add_fd env n Unix.(openfile f [ O_RDONLY ] perm)
      else fail (Printf.sprintf "%s: No such file or directory" f)
    | RedirOtp (n, w) ->
      to_str s_env w
      >>= fun f -> env_add_fd env n Unix.(openfile f [ O_CREAT; O_WRONLY ] perm)
    | AppendOtp (n, w) ->
      to_str s_env w
      >>= fun f -> env_add_fd env n Unix.(openfile f [ O_CREAT; O_WRONLY; O_APPEND ] perm)
    | DuplInp (n, w) -> to_str s_env w >>= fun s -> env_dup_fd env n s
    | DuplOtp (n, w) -> to_str s_env w >>= fun s -> env_dup_fd env n s

  and ev_redirs env ?(s_env = env) rs =
    List.fold_left (fun acc r -> acc >>= fun env -> ev_redir env ~s_env r) (return env) rs

  (** Evaluate command *)
  and ev_cmd env =
    let expand env ws =
      ev_words env ws
      >>= function
      | _, cmd :: tl -> return (cmd, cmd :: tl)
      | _, [] -> fail "Simple command expanded to nothing"
    in
    let named_args argv =
      List.mapi (fun i e -> string_of_int i, IndArray (IMap.singleton 0 e)) argv
    in
    let run_script env cmd argv =
      let try_read =
        let read_from s =
          if Sys.file_exists s && not (Sys.is_directory s)
          then (
            let ch = open_in s in
            try Some (really_input_string ch (in_channel_length ch)) with
            | Sys_error _ -> None
            | End_of_file -> None)
          else None
        in
        function
        | s when Filename.dirname s = "." -> read_from (Filename.basename s)
        | s when Filename.dirname s = "" -> read_from s
        | _ -> None
      in
      match try_read cmd with
      | Some s ->
        (match Parser.parse_result s with
        | Ok script ->
          ev_script { env with vars = SMap.add_list (named_args argv) env.vars } script
          >>| fun { retcode } -> exit retcode
        | Error e -> fail (Printf.sprintf "%s: syntax error %s" cmd e))
      | None -> fail "Script file not found"
    in
    let run_exec env cmd argv =
      let vars_to_strs vars =
        SMap.fold
          (fun k var acc ->
            let v =
              match var with
              | IndArray vs ->
                (match IMap.find_opt 0 vs with
                | Some v -> v
                | None -> "")
              | AssocArray vs ->
                (match SMap.find_opt "0" vs with
                | Some v -> v
                | None -> "")
            in
            String.concat "=" [ k; v ] :: acc)
          vars
          []
      in
      let open Unix in
      IMap.iter
        (fun n fd ->
          match n with
          | 0 -> dup2 fd stdin
          | 1 -> dup2 fd stdout
          | 2 -> dup2 fd stderr
          (* Custom file descriptors for external executables are not supported *)
          | _ -> ())
        env.chs;
      try
        execvpe
          cmd
          (Array.of_list argv)
          (Array.append (Array.of_list (vars_to_strs env.vars)) (environment ()))
      with
      | Unix_error (ENOENT, _, _) ->
        Builtins.print_err env.chs (Printf.sprintf "%s: command not found" cmd);
        exit 127
      | Unix_error (ENOEXEC, _, _) | Unix_error (EUNKNOWNERR 26, _, _) ->
        Builtins.print_err env.chs (Printf.sprintf "%s: command not executable" cmd);
        exit 126
    in
    function
    | Simple ((_ :: _ as assignts), [], _) ->
      ev_assignts env assignts >>| fun env -> { env with retcode = 0 }
    | Simple (assignts, ws, rs) ->
      expand env ws
      >>= fun (cmd, argv) ->
      ev_redirs { empty_env with chs = env.chs } ~s_env:env rs
      >>= fun n_env ->
      ev_assignts n_env ~s_env:env assignts
      >>= fun n_env ->
      (match get_fun cmd env, Builtins.find cmd with
      | Some (b, rs), _ ->
        let n_env = merge_envs env n_env in
        ev_redirs n_env rs
        >>= fun n_env ->
        ev_compound { n_env with vars = SMap.add_list (named_args argv) n_env.vars } b
        >>| fun { retcode } -> { env with retcode }
      | None, Some f -> return { env with retcode = f argv n_env.chs }
      | None, None ->
        let open Unix in
        let pid = fork () in
        if pid = 0
        then run_script n_env cmd argv <|> fun () -> run_exec n_env cmd argv
        else (
          let rec wait () =
            try
              let _, status = Unix.waitpid [] pid in
              return
                (match status with
                | WEXITED retcode -> { env with retcode }
                | WSIGNALED n | WSTOPPED n -> { env with retcode = 128 + n })
            with
            | Sys.Break ->
              (try kill pid Sys.sigint with
              | Unix_error (EACCES, _, _) ->
                Builtins.print_err env.chs "EACCES: cannot interrupt the current process");
              wait ()
          in
          Fun.protect
            ~finally:(fun () -> Sys.catch_break false)
            (fun () ->
              Sys.catch_break true;
              wait ())))
    | Compound (c, rs) ->
      ev_redirs env rs
      >>= fun n_env ->
      ev_compound n_env c >>| fun { vars; retcode } -> { env with vars; retcode }

  (** Evaluate pipeline list *)
  and ev_pipe_list env = function
    | Pipe p -> ev_pipe env p
    | PipeAndList (hd, tl) ->
      ev_pipe env hd
      >>= fun env -> if env.retcode = 0 then ev_pipe_list env tl else return env
    | PipeOrList (hd, tl) ->
      ev_pipe env hd
      >>= fun env -> if env.retcode <> 0 then ev_pipe_list env tl else return env

  (** Evaluate pipeline *)
  and ev_pipe env =
    let get_retcode neg rc = if neg then if rc = 0 then 1 else 0 else rc in
    let rec helper env cl c = function
      | hd :: tl ->
        let rd, wr = Unix.pipe () in
        ev_cmd { env with chs = IMap.add 1 wr env.chs } c
        >>= fun _ ->
        if cl then Unix.close (IMap.find 0 env.chs);
        Unix.close wr;
        helper { env with chs = IMap.add 0 rd env.chs } true hd tl
      | [] ->
        ev_cmd env c
        >>| fun { retcode } ->
        if cl then Unix.close (IMap.find 0 env.chs);
        retcode
    in
    function
    | neg, cmd, [] ->
      (* A single command should affect the environment *)
      ev_cmd env cmd >>| fun env -> { env with retcode = get_retcode neg env.retcode }
    | neg, c, cs ->
      (* A pipeline should only affect the return code *)
      helper env false c cs
      >>| fun retcode -> { env with retcode = get_retcode neg retcode }

  (** Evaluate compound command *)
  and ev_compound env = function
    | Group pls -> ev_group env pls
    | While (cnd, body) -> ev_while_loop env cnd body
    | ForList (name, ws, body) -> ev_for_list_loop env name ws body
    | ForExpr (a1, a2, a3, body) -> ev_for_expr_loop env a1 a2 a3 body
    | If (cnd, cns, alt) -> ev_if_stmt env cnd cns alt
    | Case (w, cs) -> ev_case_stmt env w cs
    | ArithmExpr a -> ev_arithm_expr env a

  (** Evaluate while loop *)
  and ev_group env =
    List.fold_left (fun acc pl -> acc >>= fun env -> ev_pipe_list env pl) (return env)

  (** Evaluate while loop *)
  and ev_while_loop env cnd body =
    (* Return code is passed so that if body was executed 0 times the return code of the
      loop is 0, but condition receives the real environment with the real return code *)
    let rec loop env retcode =
      ev_pipe_list env cnd
      >>= fun cnd_env ->
      if cnd_env.retcode = 0
      then ev_pipe_list cnd_env body >>= fun env -> loop env env.retcode
      else return { cnd_env with retcode }
    in
    loop env 0

  (** Evaluate for loop (list form) *)
  and ev_for_list_loop env name ws body =
    ev_words env ws
    >>= fun (env, ss) ->
    (* Return code is passed so that if body was executed 0 times the return code of the
      loop is 0, but body receives the real environment with the real return code *)
    let rec loop env retcode = function
      | hd :: tl ->
        ev_pipe_list (set_var name (IndArray (IMap.singleton 0 hd)) env) body
        >>= fun env -> loop env env.retcode tl
      | [] -> return { env with retcode }
    in
    loop env 0 ss

  (** Evaluate for loop (expression form) *)
  and ev_for_expr_loop env a1 a2 a3 body =
    ev_arithm env a1
    >>= fun (env, _) ->
    (* Return code is passed to save the return code of the last body's execution *)
    let rec loop env retcode =
      ev_arithm env a2
      >>= fun (env, v) ->
      if v <> 0
      then
        ev_pipe_list env body
        >>= fun env -> ev_arithm env a3 >>= fun (new_env, _) -> loop new_env env.retcode
      else return { env with retcode }
    in
    loop env 0

  (** Evaluate if statement *)
  and ev_if_stmt env cnd cns alt =
    ev_pipe_list env cnd
    >>= fun cnd_env ->
    if cnd_env.retcode = 0
    then ev_pipe_list cnd_env cns
    else (
      match alt with
      | Some alt -> ev_pipe_list cnd_env alt
      | None -> return { cnd_env with retcode = 0 })

  (** Evaluate case statement *)
  and ev_case_stmt env w cs =
    ev_word env w
    >>= fun (env, ss) ->
    match ss with
    | [ s ] ->
      let rec helper env = function
        | (ws, item) :: tl ->
          ev_words
            ~c:(function
              | [ _ ] -> true
              | _ -> false)
            ~e:"Illegal expansion in case statement item"
            env
            ws
          >>= fun (env, ptrns) ->
          (match
             List.filter
               (fun p -> Re.(execp (Glob.glob ~anchored:true p |> compile) s))
               ptrns
           with
          | [] -> helper env tl
          | _ -> ev_pipe_list env item)
        | [] -> return { env with retcode = 0 }
      in
      helper env cs
    | _ -> fail "Illegal expansion in case statement"

  (** Evaluate arithmetic expression *)
  and ev_arithm_expr env a =
    ev_arithm env a
    >>| fun (env, n) ->
    if n <> 0 then { env with retcode = 0 } else { env with retcode = 1 }

  (** Evaluate function *)
  and ev_func env ((name, body, rs) : func) =
    return { (set_fun name (body, rs) env) with retcode = 0 }

  (** Evaluate script element *)
  and ev_script_elem env = function
    | Func f -> ev_func env f
    | Pipes ps -> ev_pipe_list env ps

  (** Evaluate Bash script *)
  and ev_script env = function
    | hd :: tl -> ev_script_elem env hd >>= fun env -> ev_script env tl
    | [] -> return env
  ;;
end

(* ----------------------------------------------- *)
(* -------------------- Tests -------------------- *)
(* ----------------------------------------------- *)

open Eval (Result)

(* -------------------- Helper functions -------------------- *)

type test_environment =
  { env : environment
  ; wr_stdin : Unix.file_descr [@printer fun fmt _ -> fprintf fmt "[...]"]
  ; rd_stdout : Unix.file_descr [@printer fun fmt _ -> fprintf fmt "[...]"]
  ; rd_stderr : Unix.file_descr [@printer fun fmt _ -> fprintf fmt "[...]"]
  }
[@@deriving show { with_path = false }]

let cmp_envs
    { vars = vs1; funs = fs1; retcode = rc1 }
    { vars = vs2; funs = fs2; retcode = rc2 }
  =
  (vs1, fs1, rc1) = (vs2, fs2, rc2)
;;

let cmp_env_pairs
    ({ vars = vs1; funs = fs1; retcode = rc1 }, r1)
    ({ vars = vs2; funs = fs2; retcode = rc2 }, r2)
  =
  (vs1, fs1, rc1, r1) = (vs2, fs2, rc2, r2)
;;

let make_test_env ?(tmpl = empty_env) () =
  let stdin, wr_stdin = Unix.pipe () in
  let rd_stdout, stdout = Unix.pipe () in
  let rd_stderr, stderr = Unix.pipe () in
  { env = { tmpl with chs = IMap.add_list [ 0, stdin; 1, stdout; 2, stderr ] tmpl.chs }
  ; wr_stdin
  ; rd_stdout
  ; rd_stderr
  }
;;

let with_test_env f test_env =
  let open Unix in
  close (IMap.find 0 test_env.env.chs);
  let res = f in
  close (IMap.find 1 test_env.env.chs);
  close (IMap.find 2 test_env.env.chs);
  close test_env.wr_stdin;
  let act_stdout = read_all test_env.rd_stdout in
  let act_stderr = read_all test_env.rd_stderr in
  close test_env.rd_stdout;
  close test_env.rd_stderr;
  res, act_stdout, act_stderr
;;

module TestMake (T : sig
  type giv_t
  type exp_t

  val pp_giv : Format.formatter -> giv_t -> unit
  val pp_res : Format.formatter -> exp_t -> unit
  val ev : environment -> giv_t -> (exp_t, string) result
  val cmp : exp_t -> exp_t -> bool
end) =
struct
  let succ_ev
      ?(tmpl = empty_env)
      ?(cnd = fun _ -> true)
      ?(giv_stdin = "")
      giv
      ?(exp_stdout = "")
      ?(exp_stderr = "")
      exp
    =
    let test_env = make_test_env ~tmpl () in
    let (_ : int) =
      Unix.write_substring test_env.wr_stdin giv_stdin 0 (String.length giv_stdin)
    in
    let res, act_stdout, act_stderr = with_test_env (T.ev test_env.env giv) test_env in
    match res with
    | Error e ->
      Printf.printf "Error: %s\n" e;
      false
    | Ok res
      when T.cmp res exp && act_stdout = exp_stdout && act_stderr = exp_stderr && cnd res
      -> true
    | Ok res ->
      print_string "\n-------------------- Input --------------------\n";
      T.pp_giv Format.std_formatter giv;
      Format.(pp_print_flush std_formatter ());
      print_string "\n----------------- Environment -----------------\n";
      pp_test_environment Format.std_formatter test_env;
      Format.(pp_print_flush std_formatter ());
      print_string "\n------------------- Expected ------------------\n";
      Printf.printf "Stdout: %S\nStderr: %S\n" exp_stdout exp_stderr;
      T.pp_res Format.std_formatter exp;
      Format.(pp_print_flush std_formatter ());
      print_string "\n-------------------- Actual -------------------\n";
      Printf.printf "Stdout: %S\nStderr: %S\n" act_stdout act_stderr;
      T.pp_res Format.std_formatter res;
      Format.(pp_print_flush std_formatter ());
      print_string "\n-----------------------------------------------\n";
      flush stdout;
      false
  ;;

  let fail_ev ?(tmpl = empty_env) ?(giv_stdin = "") giv exp =
    let test_env = make_test_env ~tmpl () in
    let (_ : int) =
      Unix.write_substring test_env.wr_stdin giv_stdin 0 (String.length giv_stdin)
    in
    let res, act_stdout, act_stderr = with_test_env (T.ev test_env.env giv) test_env in
    match res with
    | Error e when e = exp -> true
    | Error e ->
      print_string "\n-------------------- Input --------------------\n";
      T.pp_giv Format.std_formatter giv;
      Format.(pp_print_flush std_formatter ());
      print_string "\n----------------- Environment -----------------\n";
      pp_test_environment Format.std_formatter test_env;
      Format.(pp_print_flush std_formatter ());
      print_string "\n--------------- Unexpected error --------------\n";
      print_string e;
      print_string "\n-----------------------------------------------\n";
      flush stdout;
      false
    | Ok res ->
      print_string "\n-------------------- Input --------------------\n";
      T.pp_giv Format.std_formatter giv;
      Format.(pp_print_flush std_formatter ());
      print_string "\n----------------- Environment -----------------\n";
      pp_test_environment Format.std_formatter test_env;
      Format.(pp_print_flush std_formatter ());
      print_string "\n-------------------- Actual -------------------\n";
      Printf.printf "Stdout: %S\nStderr: %S\n" act_stdout act_stderr;
      T.pp_res Format.std_formatter res;
      Format.(pp_print_flush std_formatter ());
      print_string "\n-----------------------------------------------\n";
      flush stdout;
      false
  ;;
end

(* -------------------- Variable -------------------- *)

open TestMake (struct
  type giv_t = var
  type exp_t = string

  let pp_giv = pp_var
  let pp_res = Format.pp_print_string
  let ev = ev_var
  let cmp = ( = )
end)

let%test _ = succ_ev ("ABC", "0") ""
let%test _ = succ_ev ("ABC", "0") ""

let%test _ =
  succ_ev
    ~tmpl:{ empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "2")) }
    ("ABC", "0")
    "2"
;;

let%test _ =
  succ_ev
    ~tmpl:
      { empty_env with
        vars = SMap.singleton "ABC" (IndArray (IMap.of_list [ 0, "a"; 1, "b"; 2, "c" ]))
      }
    ("ABC", "0")
    "a"
;;

let%test _ =
  succ_ev
    ~tmpl:
      { empty_env with
        vars = SMap.singleton "ABC" (IndArray (IMap.of_list [ 0, "a"; 1, "b"; 2, "c" ]))
      }
    ("ABC", "1")
    "b"
;;

let%test _ =
  succ_ev
    ~tmpl:
      { empty_env with
        vars = SMap.singleton "ABC" (IndArray (IMap.of_list [ 0, "a"; 1, "b"; 2, "c" ]))
      }
    ("ABC", "3")
    ""
;;

let%test _ =
  succ_ev
    ~tmpl:
      { empty_env with
        vars =
          SMap.singleton
            "ABC"
            (AssocArray (SMap.of_list [ "a", "a1"; "b", "b1"; "0", "01" ]))
      }
    ("ABC", "0")
    "01"
;;

let%test _ =
  succ_ev
    ~tmpl:
      { empty_env with
        vars =
          SMap.singleton
            "ABC"
            (AssocArray (SMap.of_list [ "a", "a1"; "b", "b1"; "0", "01" ]))
      }
    ("ABC", "b")
    "b1"
;;

let%test _ =
  succ_ev
    ~tmpl:
      { empty_env with
        vars =
          SMap.singleton
            "ABC"
            (AssocArray (SMap.of_list [ "a", "a1"; "b", "b1"; "0", "01" ]))
      }
    ("ABC", "!")
    ""
;;

(* -------------------- Arithmetic -------------------- *)

open TestMake (struct
  type giv_t = arithm
  type exp_t = environment * int

  let pp_giv = pp_arithm
  let pp_res fmt (env, n) = Format.fprintf fmt "%a@\n%i" pp_environment env n
  let ev = ev_arithm
  let cmp = cmp_env_pairs
end)

let%test _ = succ_ev (Plus (Num 1, Num 2)) (empty_env, 3)
let%test _ = succ_ev (Div (Num 1, Num 3)) (empty_env, 0)
let%test _ = succ_ev (Div (Num 2, Num 3)) (empty_env, 0)
let%test _ = succ_ev (Less (Num 1, Num 2)) (empty_env, 1)
let%test _ = succ_ev (Div (NEqual (Num 1, Num 2), Greater (Num 3, Num 1))) (empty_env, 1)

let%test _ =
  succ_ev
    (ArithmAssignt (("X", "0"), Plus (Num 1, Num 2)))
    ({ empty_env with vars = SMap.singleton "X" (IndArray (IMap.singleton 0 "3")) }, 3)
;;

let%test _ = fail_ev (Div (Num 1, Num 0)) "Division by 0"

(* -------------------- Parameter expansion -------------------- *)

open TestMake (struct
  type giv_t = param_exp
  type exp_t = environment * string

  let pp_giv = pp_param_exp
  let pp_res fmt (env, s) = Format.fprintf fmt "%a@\n%s" pp_environment env s
  let ev = ev_param_exp
  let cmp = cmp_env_pairs
end)

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "abc")) }
  in
  succ_ev ~tmpl (Param ("ABC", "0")) (tmpl, "abc")
;;

let%test _ = succ_ev (Param ("ABC", "0")) (empty_env, "")

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "3" (IndArray (IMap.singleton 0 "123")) }
  in
  succ_ev ~tmpl (Param ("3", "0")) (tmpl, "123")
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "12345")) }
  in
  succ_ev ~tmpl (Length ("ABC", "0")) (tmpl, "5")
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "")) }
  in
  succ_ev ~tmpl (Length ("ABC", "0")) (tmpl, "0")
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "01234")) }
  in
  succ_ev ~tmpl (Substring (("ABC", "0"), Num 0, Some (Num 3))) (tmpl, "012")
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "01234")) }
  in
  succ_ev ~tmpl (Substring (("ABC", "0"), Num 0, Some (Num 5))) (tmpl, "01234")
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "01234")) }
  in
  succ_ev ~tmpl (Substring (("ABC", "0"), Num 0, Some (Num 7))) (tmpl, "01234")
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "01234")) }
  in
  succ_ev ~tmpl (Substring (("ABC", "0"), Num 2, Some (Num 2))) (tmpl, "23")
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "01234")) }
  in
  succ_ev ~tmpl (Substring (("ABC", "0"), Num 2, Some (Num 0))) (tmpl, "")
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "01234")) }
  in
  succ_ev ~tmpl (Substring (("ABC", "0"), Num 2, None)) (tmpl, "234")
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "01234")) }
  in
  succ_ev ~tmpl (Substring (("ABC", "0"), Num (-3), Some (Num 2))) (tmpl, "23")
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "01234")) }
  in
  succ_ev ~tmpl (Substring (("ABC", "0"), Num (-1), Some (Num 2))) (tmpl, "4")
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "01234")) }
  in
  succ_ev ~tmpl (Substring (("ABC", "0"), Num (-6), Some (Num 3))) (tmpl, "")
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "01234")) }
  in
  succ_ev ~tmpl (Substring (("ABC", "0"), Num (-3), Some (Num (-2)))) (tmpl, "2")
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "01234")) }
  in
  succ_ev ~tmpl (Substring (("ABC", "0"), Num (-3), Some (Num (-3)))) (tmpl, "")
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "01234")) }
  in
  succ_ev ~tmpl (Substring (("ABC", "0"), Num 0, Some (Num (-5)))) (tmpl, "")
;;

let%test _ =
  fail_ev
    ~tmpl:
      { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "01234")) }
    (Substring (("ABC", "0"), Num 4, Some (Num (-3))))
    "substring expression < 0"
;;

let%test _ =
  fail_ev
    ~tmpl:
      { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "01234")) }
    (Substring (("ABC", "0"), Num 0, Some (Num (-6))))
    "substring expression < 0"
;;

let%test _ =
  fail_ev
    ~tmpl:
      { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "01234")) }
    (Substring (("ABC", "0"), Div (Num 1, Num 0), None))
    "Division by 0"
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "01234")) }
  in
  succ_ev ~tmpl (CutMinBeg (("ABC", "0"), "*")) (tmpl, "01234")
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "01234")) }
  in
  succ_ev ~tmpl (CutMinBeg (("ABC", "0"), "?")) (tmpl, "1234")
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "01234")) }
  in
  succ_ev ~tmpl (CutMinBeg (("ABC", "0"), "[0-9][0-9]")) (tmpl, "234")
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "01234")) }
  in
  succ_ev ~tmpl (CutMinBeg (("ABC", "0"), "23")) (tmpl, "01234")
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "01234")) }
  in
  succ_ev ~tmpl (CutMinBeg (("ABC", "0"), "01")) (tmpl, "234")
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "01234")) }
  in
  succ_ev ~tmpl (CutMaxBeg (("ABC", "0"), "*")) (tmpl, "")
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "01234")) }
  in
  succ_ev ~tmpl (CutMinEnd (("ABC", "0"), "?")) (tmpl, "0123")
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "01234")) }
  in
  succ_ev ~tmpl (CutMinEnd (("ABC", "0"), "*")) (tmpl, "01234")
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "01234")) }
  in
  succ_ev ~tmpl (CutMaxEnd (("ABC", "0"), "?")) (tmpl, "0123")
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "01234")) }
  in
  succ_ev ~tmpl (CutMaxEnd (("ABC", "0"), "*")) (tmpl, "")
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "01234")) }
  in
  succ_ev ~tmpl (SubstOne (("ABC", "0"), "?", "a")) (tmpl, "a1234")
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "01234")) }
  in
  succ_ev ~tmpl (SubstOne (("ABC", "0"), "*", "a")) (tmpl, "a")
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "")) }
  in
  succ_ev ~tmpl (SubstOne (("ABC", "0"), "*", "a")) (tmpl, "a")
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "01234")) }
  in
  succ_ev ~tmpl (SubstAll (("ABC", "0"), "?", "a")) (tmpl, "aaaaa")
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "01234")) }
  in
  succ_ev ~tmpl (SubstAll (("ABC", "0"), "*", "a")) (tmpl, "a")
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "abacab")) }
  in
  succ_ev ~tmpl (SubstAll (("ABC", "0"), "a", "heh")) (tmpl, "hehbhehchehb")
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "01234")) }
  in
  succ_ev ~tmpl (SubstBeg (("ABC", "0"), "*", "a")) (tmpl, "a")
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "abcabab")) }
  in
  succ_ev ~tmpl (SubstBeg (("ABC", "0"), "ab", "!")) (tmpl, "!cabab")
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "01234")) }
  in
  succ_ev ~tmpl (SubstEnd (("ABC", "0"), "*", "a")) (tmpl, "a")
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "abcabab")) }
  in
  succ_ev ~tmpl (SubstEnd (("ABC", "0"), "ab", "!")) (tmpl, "abcab!")
;;

(* -------------------- Filename expansion -------------------- *)

open TestMake (struct
  type giv_t = string
  type exp_t = string list

  let pp_giv = Format.pp_print_string
  let pp_res = Format.(pp_print_list ~pp_sep:pp_print_newline pp_print_string)
  let ev = ev_filename_exp
  let cmp = ( = )
end)

let cwd_satisfy c =
  let cwd = Sys.getcwd () in
  cwd
  |> Sys.readdir
  |> Array.to_list
  |> List.filter (fun f ->
         Sys.file_exists (Filename.concat cwd f) && (not (f.[0] = '.')) && c f)
  |> List.sort String.compare
;;

let%test _ = succ_ev "*.ml" (cwd_satisfy (fun f -> Filename.check_suffix f ".ml"))

(* -------------------- Word -------------------- *)

open TestMake (struct
  type giv_t = word
  type exp_t = environment * string list

  let pp_giv = pp_word

  let pp_res fmt (env, ss) =
    Format.(
      fprintf
        fmt
        "%a%a"
        pp_environment
        env
        (pp_print_list ~pp_sep:pp_print_newline pp_print_string)
        ss)
  ;;

  let ev = ev_word
  let cmp = cmp_env_pairs
end)

let%test _ =
  succ_ev (BraceExp [ ParamExp (Param ("X", "0")); Word "y" ]) (empty_env, [ ""; "y" ])
;;

let%test _ =
  let tmpl =
    { empty_env with vars = SMap.singleton "M" (IndArray (IMap.singleton 0 "meow")) }
  in
  succ_ev ~tmpl (ParamExp (Param ("M", "0"))) (tmpl, [ "meow" ])
;;

let%test _ =
  succ_ev
    ~tmpl:empty_env
    (CmdSubst (Simple ([], [ Word "echo"; Word "123 45" ], [])))
    (empty_env, [ "123 45" ])
;;

let%test _ = succ_ev (ArithmExp (Num 5)) (empty_env, [ "5" ])
let%test _ = fail_ev (ArithmExp (Div (Num 5, Num 0))) "Division by 0"

let%test _ =
  succ_ev (FilenameExp "*.*") (empty_env, cwd_satisfy (fun f -> String.contains f '.'))
;;

let%test _ = succ_ev (Word "hey") (empty_env, [ "hey" ])

let%test _ =
  succ_ev
    (DoubleQuotes
       [ Word "a"; CmdSubst (Simple ([], [ Word "echo"; Word "b c" ], [])); Word "d" ])
    (empty_env, [ "ab cd" ])
;;

let%test _ =
  succ_ev
    (DoubleQuotes
       [ Word "a"
       ; CmdSubst (Simple ([], [ Word "echo"; Word "b c" ], []))
       ; ArithmExp (Plus (Num 1, Num 2))
       ])
    (empty_env, [ "ab c3" ])
;;

(* -------------------- Assignment -------------------- *)

open TestMake (struct
  type giv_t = assignt
  type exp_t = environment

  let pp_giv = pp_assignt
  let pp_res = pp_environment
  let ev env assignt = ev_assignt env assignt
  let cmp = cmp_envs
end)

let%test _ =
  succ_ev
    (SimpleAssignt (("X", "0"), Word "x"))
    { empty_env with vars = SMap.singleton "X" (IndArray (IMap.singleton 0 "x")) }
;;

let%test _ =
  fail_ev
    (SimpleAssignt (("X", "0"), BraceExp [ Word "x"; Word "y" ]))
    "Illegal expansion in simple assignment"
;;

let%test _ =
  succ_ev
    (SimpleAssignt (("X", "1"), Word "x"))
    { empty_env with vars = SMap.singleton "X" (IndArray (IMap.singleton 1 "x")) }
;;

let%test _ =
  succ_ev
    (SimpleAssignt (("X", "a"), Word "x"))
    { empty_env with vars = SMap.singleton "X" (AssocArray (SMap.singleton "a" "x")) }
;;

let%test _ =
  fail_ev
    (SimpleAssignt (("X", "1"), BraceExp [ Word "x"; Word "y" ]))
    "Illegal expansion in simple assignment"
;;

let%test _ =
  succ_ev
    (IndArrAssignt ("X", [ Word "x" ]))
    { empty_env with vars = SMap.singleton "X" (IndArray (IMap.singleton 0 "x")) }
;;

let%test _ =
  succ_ev
    (IndArrAssignt ("X", [ Word "x"; BraceExp [ Word "y"; Word "z" ] ]))
    { empty_env with
      vars = SMap.singleton "X" (IndArray (IMap.of_list [ 0, "x"; 1, "y"; 2, "z" ]))
    }
;;

let%test _ =
  succ_ev
    (AssocArrAssignt ("X", [ "x", Word "y" ]))
    { empty_env with vars = SMap.singleton "X" (AssocArray (SMap.singleton "x" "y")) }
;;

let%test _ =
  fail_ev
    (AssocArrAssignt ("X", [ "x", BraceExp [ Word "y"; Word "z" ] ]))
    "Illegal expansion in associative array assignment"
;;

let%test _ =
  succ_ev
    ~tmpl:{ empty_env with vars = SMap.singleton "X" (IndArray (IMap.singleton 0 "x")) }
    (SimpleAssignt (("X", "0"), Word "y"))
    { empty_env with vars = SMap.singleton "X" (IndArray (IMap.singleton 0 "y")) }
;;

let%test _ =
  succ_ev
    ~tmpl:{ empty_env with vars = SMap.singleton "X" (IndArray (IMap.singleton 0 "x")) }
    (SimpleAssignt (("X", "0"), Word "y"))
    { empty_env with vars = SMap.singleton "X" (IndArray (IMap.singleton 0 "y")) }
;;

let%test _ =
  succ_ev
    ~tmpl:{ empty_env with vars = SMap.singleton "X" (IndArray (IMap.singleton 1 "x")) }
    (SimpleAssignt (("X", "0"), Word "y"))
    { empty_env with
      vars = SMap.singleton "X" (IndArray (IMap.of_list [ 1, "x"; 0, "y" ]))
    }
;;

let%test _ =
  succ_ev
    ~tmpl:
      { empty_env with vars = SMap.singleton "X" (AssocArray (SMap.singleton "0" "x")) }
    (SimpleAssignt (("X", "0"), Word "y"))
    { empty_env with vars = SMap.singleton "X" (AssocArray (SMap.singleton "0" "y")) }
;;

let%test _ =
  succ_ev
    ~tmpl:
      { empty_env with vars = SMap.singleton "X" (AssocArray (SMap.singleton "a" "x")) }
    (SimpleAssignt (("X", "0"), Word "y"))
    { empty_env with
      vars = SMap.singleton "X" (AssocArray (SMap.of_list [ "a", "x"; "0", "y" ]))
    }
;;

let%test _ =
  succ_ev
    ~tmpl:{ empty_env with vars = SMap.singleton "X" (IndArray (IMap.singleton 0 "x")) }
    (SimpleAssignt (("X", "1"), Word "y"))
    { empty_env with
      vars = SMap.singleton "X" (IndArray (IMap.of_list [ 0, "x"; 1, "y" ]))
    }
;;

let%test _ =
  succ_ev
    ~tmpl:{ empty_env with vars = SMap.singleton "X" (IndArray (IMap.singleton 1 "x")) }
    (SimpleAssignt (("X", "1"), Word "y"))
    { empty_env with vars = SMap.singleton "X" (IndArray (IMap.singleton 1 "y")) }
;;

let%test _ =
  succ_ev
    ~tmpl:
      { empty_env with vars = SMap.singleton "X" (AssocArray (SMap.singleton "1" "x")) }
    (SimpleAssignt (("X", "1"), Word "y"))
    { empty_env with vars = SMap.singleton "X" (AssocArray (SMap.singleton "1" "y")) }
;;

let%test _ =
  succ_ev
    ~tmpl:{ empty_env with vars = SMap.singleton "X" (IndArray (IMap.singleton 0 "x")) }
    (SimpleAssignt (("X", "a"), Word "y"))
    { empty_env with vars = SMap.singleton "X" (IndArray (IMap.singleton 0 "y")) }
;;

let%test _ =
  succ_ev
    ~tmpl:{ empty_env with vars = SMap.singleton "X" (IndArray (IMap.singleton 1 "x")) }
    (SimpleAssignt (("X", "a"), Word "y"))
    { empty_env with
      vars = SMap.singleton "X" (IndArray (IMap.of_list [ 1, "x"; 0, "y" ]))
    }
;;

let%test _ =
  succ_ev
    ~tmpl:
      { empty_env with vars = SMap.singleton "X" (AssocArray (SMap.singleton "1" "x")) }
    (SimpleAssignt (("X", "a"), Word "y"))
    { empty_env with
      vars = SMap.singleton "X" (AssocArray (SMap.of_list [ "1", "x"; "a", "y" ]))
    }
;;

let%test _ =
  succ_ev
    ~tmpl:{ empty_env with vars = SMap.singleton "X" (IndArray (IMap.singleton 0 "x")) }
    (IndArrAssignt ("X", [ Word "x" ]))
    { empty_env with vars = SMap.singleton "X" (IndArray (IMap.singleton 0 "x")) }
;;

let%test _ =
  succ_ev
    ~tmpl:{ empty_env with vars = SMap.singleton "X" (IndArray (IMap.singleton 0 "x")) }
    (AssocArrAssignt ("X", [ "x", Word "y" ]))
    { empty_env with vars = SMap.singleton "X" (AssocArray (SMap.singleton "x" "y")) }
;;

(* -------------------- Redirection -------------------- *)

open TestMake (struct
  type giv_t = redir
  type exp_t = environment

  let pp_giv = pp_redir
  let pp_res = pp_environment
  let ev env redir = ev_redir env redir
  let cmp = cmp_envs
end)

let test_file = "INTEPRETER_TEST_FILE"

(*
Important moments in these tests:
1) Call to open_out is required to open and keep the fd open while comparing environments
2) It is pointless to change file descriptors 0-2 in the template environment as they are
rewritten later in testing
*)
let with_test_file f =
  let oc = open_out test_file in
  let ic = open_in test_file in
  Fun.protect
    ~finally:(fun () ->
      close_out_noerr oc;
      Sys.remove test_file)
    (fun () -> f ic oc)
;;

let%test _ =
  with_test_file (fun _ _ ->
      succ_ev
        (RedirInp (0, Word test_file))
        empty_env
        ~cnd:(fun env -> Unix.fstat (IMap.find 0 env.chs) = Unix.stat test_file))
;;

let%test _ =
  with_test_file (fun _ _ ->
      succ_ev
        (RedirOtp (1, Word test_file))
        empty_env
        ~cnd:(fun env -> Unix.fstat (IMap.find 1 env.chs) = Unix.stat test_file))
;;

let%test _ =
  with_test_file (fun _ _ ->
      succ_ev
        (AppendOtp (1, Word test_file))
        empty_env
        ~cnd:(fun env -> Unix.fstat (IMap.find 1 env.chs) = Unix.stat test_file))
;;

let%test _ =
  with_test_file (fun ic _ ->
      let fd = Unix.descr_of_in_channel ic in
      let tmpl = { empty_env with chs = IMap.add 4 fd empty_env.chs } in
      succ_ev
        ~tmpl
        (DuplInp (0, Word "4"))
        tmpl
        ~cnd:(fun env -> Unix.fstat (IMap.find 0 env.chs) = Unix.fstat fd))
;;

let%test _ =
  with_test_file (fun _ oc ->
      let fd = Unix.descr_of_out_channel oc in
      let tmpl = { empty_env with chs = IMap.add 4 fd empty_env.chs } in
      succ_ev
        ~tmpl
        (DuplOtp (1, Word "4"))
        tmpl
        ~cnd:(fun env -> Unix.fstat (IMap.find 1 env.chs) = Unix.fstat fd))
;;

let%test _ =
  fail_ev
    (RedirInp (0, BraceExp [ Word "a"; Word "b" ]))
    "Illegal expansion in redirection"
;;

let%test _ =
  fail_ev
    (RedirInp (0, Word test_file))
    (Printf.sprintf "%s: No such file or directory" test_file)
;;

let%test _ = fail_ev (DuplInp (0, Word "4")) (Printf.sprintf "%i: Bad file descriptor" 4)
let%test _ = fail_ev (DuplInp (0, Word "a")) (Printf.sprintf "%s: ambiguous redirect" "a")

(* -------------------- Command -------------------- *)

open TestMake (struct
  type giv_t = cmd
  type exp_t = environment

  let pp_giv = pp_cmd
  let pp_res = pp_environment
  let ev = ev_cmd
  let cmp = cmp_envs
end)

let%test _ =
  succ_ev
    (Simple ([ SimpleAssignt (("X", "0"), Word "a") ], [], []))
    { empty_env with vars = SMap.singleton "X" (IndArray (IMap.singleton 0 "a")) }
;;

let%test _ =
  succ_ev (Simple ([], [ Word "echo"; Word "123" ], [])) empty_env ~exp_stdout:"123\n"
;;

let%test _ =
  succ_ev
    (Simple ([], [ Word "echo"; Word "123" ], [ DuplOtp (1, Word "2") ]))
    empty_env
    ~exp_stderr:"123\n"
;;

let%test _ =
  succ_ev
    (Simple ([], [ Word "echo"; Word "1"; BraceExp [ ArithmExp (Num 2); Word "3" ] ], []))
    empty_env
    ~exp_stdout:"1 2 3\n"
;;

let%test _ =
  let tmpl =
    { empty_env with
      funs =
        SMap.singleton
          "say_meow"
          (Group [ Pipe (false, Simple ([], [ Word "echo"; Word "meow" ], []), []) ], [])
    }
  in
  succ_ev ~tmpl (Simple ([], [ Word "say_meow" ], [])) tmpl ~exp_stdout:"meow\n"
;;

let%test _ =
  let tmpl =
    { empty_env with
      funs =
        SMap.singleton
          "cat"
          (Group [ Pipe (false, Simple ([], [ Word "echo"; Word "meow" ], []), []) ], [])
    }
  in
  succ_ev ~tmpl (Simple ([], [ Word "cat" ], [])) tmpl ~exp_stdout:"meow\n"
;;

let%test _ =
  let tmpl =
    { empty_env with
      funs =
        SMap.singleton
          "say_meow"
          ( Group [ Pipe (false, Simple ([], [ Word "echo"; Word "meow" ], []), []) ]
          , [ DuplOtp (1, Word "2") ] )
    }
  in
  succ_ev ~tmpl (Simple ([], [ Word "say_meow" ], [])) tmpl ~exp_stderr:"meow\n"
;;

let%test _ =
  succ_ev
    (Compound
       ( If
           ( Pipe (false, Compound (ArithmExpr (Num 1), []), [])
           , Pipe (false, Simple ([], [ Word "echo"; Word "meow" ], []), [])
           , None )
       , [ DuplOtp (1, Word "2") ] ))
    empty_env
    ~exp_stderr:"meow\n"
;;

let%test _ =
  with_test_file (fun _ oc ->
      output_string oc "#!/bin/sh\necho sample!";
      close_out oc;
      succ_ev
        (Simple ([], [ Word ("./" ^ test_file) ], []))
        empty_env
        ~exp_stdout:"sample!\n")
;;

let%test _ =
  with_test_file (fun _ oc ->
      output_string oc "#!/bin/sh\necho sample!";
      close_out oc;
      succ_ev
        (Simple ([], [ Word ("./" ^ test_file) ], []))
        empty_env
        ~exp_stdout:"sample!\n")
;;

let%test _ =
  with_test_file (fun _ oc ->
      output_string oc "#!/bin/sh\necho sample!";
      close_out oc;
      succ_ev
        (Simple ([], [ Word test_file ], [ DuplOtp (1, Word "2") ]))
        empty_env
        ~exp_stderr:"sample!\n")
;;

let%test _ =
  with_test_file (fun _ oc ->
      output_string oc "#!/bin/sh\necho $A";
      close_out oc;
      succ_ev
        (Simple ([ SimpleAssignt (("A", "0"), Word "123") ], [ Word test_file ], []))
        empty_env
        ~exp_stdout:"123\n")
;;

let%test _ =
  with_test_file (fun _ oc ->
      output_string oc "#!/bin/sh\necho $A $X";
      close_out oc;
      let tmpl =
        { empty_env with vars = SMap.singleton "X" (IndArray (IMap.singleton 0 "x")) }
      in
      succ_ev
        ~tmpl
        (Simple ([ SimpleAssignt (("A", "0"), Word "123") ], [ Word test_file ], []))
        tmpl
        ~exp_stdout:"123\n")
;;

(* -------------------- Compound -------------------- *)

open TestMake (struct
  type giv_t = compound
  type exp_t = environment

  let pp_giv = pp_compound
  let pp_res = pp_environment
  let ev = ev_compound
  let cmp = cmp_envs
end)

(* Group *)

let%test _ =
  succ_ev
    ~tmpl:{ empty_env with retcode = 1 }
    (Group [ Pipe (false, Compound (ArithmExpr (Num 1), []), []) ])
    empty_env
;;

let%test _ =
  succ_ev
    ~tmpl:{ empty_env with vars = SMap.singleton "X" (IndArray (IMap.singleton 0 "0")) }
    (Group
       [ Pipe (false, Compound (ArithmExpr (ArithmAssignt (("X", "0"), Num 5)), []), []) ])
    { empty_env with vars = SMap.singleton "X" (IndArray (IMap.singleton 0 "5")) }
;;

let%test _ =
  succ_ev
    (Group
       [ Pipe (false, Simple ([ SimpleAssignt (("X", "0"), Word "1") ], [], []), [])
       ; Pipe (true, Simple ([ SimpleAssignt (("X", "0"), Word "2") ], [], []), [])
       ])
    { empty_env with
      vars = SMap.singleton "X" (IndArray (IMap.singleton 0 "2"))
    ; retcode = 1
    }
;;

(* While loop *)

let%test _ =
  succ_ev
    ~tmpl:{ empty_env with retcode = 1 }
    (While
       ( Pipe (false, Compound (ArithmExpr (Num 0), []), [])
       , Pipe (false, Simple ([ SimpleAssignt (("X", "0"), Word "1") ], [], []), []) ))
    empty_env
;;

let%test _ =
  succ_ev
    ~tmpl:{ empty_env with vars = SMap.singleton "X" (IndArray (IMap.singleton 0 "0")) }
    (While
       ( Pipe (false, Compound (ArithmExpr (Less (Var ("X", "0"), Num 5)), []), [])
       , Pipe
           ( false
           , Simple
               ( [ SimpleAssignt (("X", "0"), ArithmExp (Plus (Var ("X", "0"), Num 1))) ]
               , []
               , [] )
           , [] ) ))
    { empty_env with vars = SMap.singleton "X" (IndArray (IMap.singleton 0 "5")) }
;;

(* For loop (list form) *)

let%test _ =
  succ_ev
    ~tmpl:{ empty_env with retcode = 1 }
    (ForList ("i", [], Pipe (false, Simple ([], [ Word "echo"; Word "meow" ], []), [])))
    { empty_env with retcode = 0 }
;;

let%test _ =
  succ_ev
    (ForList
       ( "i"
       , [ Word "a"; Word "b"; BraceExp [ Word "c"; Word "d" ] ]
       , Pipe (false, Simple ([], [ Word "echo"; ParamExp (Param ("i", "0")) ], []), [])
       ))
    { empty_env with vars = SMap.singleton "i" (IndArray (IMap.singleton 0 "d")) }
    ~exp_stdout:"a\nb\nc\nd\n"
;;

(* For loop (expression form) *)

let%test _ =
  succ_ev
    ~tmpl:{ empty_env with retcode = 1 }
    (ForExpr
       ( Num 1
       , Num 0
       , ArithmAssignt (("i", "0"), Num 2)
       , Pipe (false, Simple ([], [ Word "echo"; Word "meow" ], []), []) ))
    empty_env
;;

let%test _ =
  succ_ev
    (ForExpr
       ( ArithmAssignt (("i", "0"), Num 0)
       , Less (Var ("i", "0"), Num 5)
       , ArithmAssignt (("i", "0"), Plus (Var ("i", "0"), Num 1))
       , Pipe (false, Simple ([], [ Word "echo"; ParamExp (Param ("i", "0")) ], []), [])
       ))
    { empty_env with vars = SMap.singleton "i" (IndArray (IMap.singleton 0 "5")) }
    ~exp_stdout:"0\n1\n2\n3\n4\n"
;;

(* If statement *)

let%test _ =
  succ_ev
    ~tmpl:{ empty_env with retcode = 1 }
    (If
       ( Pipe (false, Compound (ArithmExpr (Num 0), []), [])
       , Pipe (false, Simple ([ SimpleAssignt (("X", "0"), Word "1") ], [], []), [])
       , None ))
    empty_env
;;

let%test _ =
  succ_ev
    ~tmpl:{ empty_env with vars = SMap.singleton "X" (IndArray (IMap.singleton 0 "123")) }
    (If
       ( Pipe (false, Compound (ArithmExpr (Equal (Var ("X", "0"), Num 123)), []), [])
       , Pipe (false, Simple ([ SimpleAssignt (("X", "0"), Word "1") ], [], []), [])
       , None ))
    { empty_env with vars = SMap.singleton "X" (IndArray (IMap.singleton 0 "1")) }
;;

let%test _ =
  succ_ev
    ~tmpl:{ empty_env with vars = SMap.singleton "X" (IndArray (IMap.singleton 0 "abc")) }
    (If
       ( Pipe (false, Compound (ArithmExpr (Equal (Var ("X", "0"), Num 123)), []), [])
       , Pipe (false, Simple ([], [ Word "echo"; Word "yes" ], []), [])
       , Some (Pipe (false, Simple ([], [ Word "echo"; Word "no" ], []), [])) ))
    { empty_env with vars = SMap.singleton "X" (IndArray (IMap.singleton 0 "abc")) }
    ~exp_stdout:"no\n"
;;

(* Case statement *)

let%test _ = succ_ev ~tmpl:{ empty_env with retcode = 1 } (Case (Word "a", [])) empty_env

let%test _ =
  succ_ev
    ~tmpl:{ empty_env with retcode = 1 }
    (Case
       ( Word "a"
       , [ [ Word "b" ], Pipe (false, Simple ([], [ Word "echo"; Word "meow" ], []), []) ]
       ))
    empty_env
;;

let%test _ =
  succ_ev
    (Case
       ( Word "abacab"
       , [ ( [ Word "mmm"; Word "a*b" ]
           , Pipe (false, Simple ([], [ Word "echo"; Word "meow" ], []), []) )
         ] ))
    empty_env
    ~exp_stdout:"meow\n"
;;

let%test _ =
  succ_ev
    (Case
       ( Word "abacab"
       , [ [ Word "cab" ], Pipe (false, Simple ([], [ Word "echo"; Word "meow" ], []), [])
         ] ))
    empty_env
;;

let%test _ =
  succ_ev
    (Case
       ( Word "a"
       , [ [ Word "a" ], Pipe (false, Simple ([], [ Word "echo"; Word "meow1" ], []), [])
         ; [ Word "a" ], Pipe (false, Simple ([], [ Word "echo"; Word "meow2" ], []), [])
         ] ))
    empty_env
    ~exp_stdout:"meow1\n"
;;

let%test _ =
  fail_ev
    (Case
       ( BraceExp [ Word "a"; Word "b" ]
       , [ [ Word "b" ], Pipe (false, Simple ([], [ Word "echo"; Word "meow" ], []), []) ]
       ))
    "Illegal expansion in case statement"
;;

let%test _ =
  fail_ev
    (Case
       ( Word "a"
       , [ ( [ BraceExp [ Word "a"; Word "b" ] ]
           , Pipe (false, Simple ([], [ Word "echo"; Word "meow" ], []), []) )
         ] ))
    "Illegal expansion in case statement item"
;;

(* -------------------- Arithmetic expression -------------------- *)

open TestMake (struct
  type giv_t = arithm
  type exp_t = environment

  let pp_giv = pp_arithm
  let pp_res = pp_environment
  let ev = ev_arithm_expr
  let cmp = cmp_envs
end)

let%test _ = succ_ev (Num 0) { empty_env with retcode = 1 }
let%test _ = succ_ev (Num 1) { empty_env with retcode = 0 }
let%test _ = succ_ev (Less (Num 1, Num 5)) { empty_env with retcode = 0 }

let%test _ =
  succ_ev
    (ArithmAssignt (("x", "0"), Num 5))
    { empty_env with
      vars = SMap.singleton "x" (IndArray (IMap.singleton 0 "5"))
    ; retcode = 0
    }
;;

let%test _ = fail_ev (Div (Num 5, Num 0)) "Division by 0"

(* -------------------- Pipeline -------------------- *)

open TestMake (struct
  type giv_t = pipe
  type exp_t = environment

  let pp_giv = pp_pipe
  let pp_res = pp_environment
  let ev = ev_pipe
  let cmp = cmp_envs
end)

let%test _ =
  succ_ev
    (false, Simple ([ SimpleAssignt (("X", "0"), Word "1") ], [], []), [])
    { empty_env with vars = SMap.singleton "X" (IndArray (IMap.singleton 0 "1")) }
;;

let%test _ =
  succ_ev (false, Compound (ArithmExpr (Num 0), []), []) { empty_env with retcode = 1 }
;;

let%test _ = succ_ev (true, Compound (ArithmExpr (Num 0), []), []) empty_env

let%test _ =
  succ_ev (true, Compound (ArithmExpr (Num 100), []), []) { empty_env with retcode = 1 }
;;

let%test _ =
  succ_ev
    ( false
    , Simple ([], [ Word "echo"; Word "meow" ], [])
    , [ Simple ([], [ Word "cat" ], [ DuplOtp (1, Word "2") ]) ] )
    empty_env
    ~exp_stderr:"meow\n"
;;

let%test _ =
  succ_ev
    ( false
    , Simple ([], [ Word "echo"; Word "meow1" ], [ DuplOtp (1, Word "2") ])
    , [ Simple ([], [ Word "echo"; Word "meow2" ], []) ] )
    empty_env
    ~exp_stdout:"meow2\n"
    ~exp_stderr:"meow1\n"
;;

let%test _ =
  succ_ev
    ( false
    , Simple ([ SimpleAssignt (("X", "0"), Word "1") ], [], [])
    , [ Simple ([], [ Word "echo"; ParamExp (Param ("X", "0")) ], []) ] )
    empty_env
    ~exp_stdout:"\n"
;;

(* -------------------- Pipeline list -------------------- *)

open TestMake (struct
  type giv_t = pipe_list
  type exp_t = environment

  let pp_giv = pp_pipe_list
  let pp_res = pp_environment
  let ev = ev_pipe_list
  let cmp = cmp_envs
end)

let%test _ =
  succ_ev
    (PipeAndList
       ( (false, Simple ([ SimpleAssignt (("X", "0"), Word "a") ], [], []), [])
       , Pipe (false, Simple ([], [ Word "echo"; ParamExp (Param ("X", "0")) ], []), [])
       ))
    { empty_env with vars = SMap.singleton "X" (IndArray (IMap.singleton 0 "a")) }
    ~exp_stdout:"a\n"
;;

let%test _ =
  succ_ev
    (PipeAndList
       ( (true, Simple ([ SimpleAssignt (("X", "0"), Word "a") ], [], []), [])
       , Pipe (false, Simple ([], [ Word "echo"; ParamExp (Param ("X", "0")) ], []), [])
       ))
    { empty_env with
      vars = SMap.singleton "X" (IndArray (IMap.singleton 0 "a"))
    ; retcode = 1
    }
;;

let%test _ =
  succ_ev
    (PipeOrList
       ( (false, Simple ([ SimpleAssignt (("X", "0"), Word "a") ], [], []), [])
       , Pipe (false, Simple ([ SimpleAssignt (("X", "0"), Word "b") ], [], []), []) ))
    { empty_env with vars = SMap.singleton "X" (IndArray (IMap.singleton 0 "a")) }
;;

let%test _ =
  succ_ev
    (PipeOrList
       ( (true, Simple ([ SimpleAssignt (("X", "0"), Word "a") ], [], []), [])
       , Pipe (false, Simple ([ SimpleAssignt (("X", "0"), Word "b") ], [], []), []) ))
    { empty_env with vars = SMap.singleton "X" (IndArray (IMap.singleton 0 "b")) }
;;

(* -------------------- Function -------------------- *)

open TestMake (struct
  type giv_t = func
  type exp_t = environment

  let pp_giv = pp_func
  let pp_res = pp_environment
  let ev = ev_func
  let cmp = cmp_envs
end)

let%test _ =
  succ_ev
    ( "say_meow"
    , Group [ Pipe (false, Simple ([], [ Word "echo"; Word "meow" ], []), []) ]
    , [] )
    { empty_env with
      funs =
        SMap.singleton
          "say_meow"
          (Group [ Pipe (false, Simple ([], [ Word "echo"; Word "meow" ], []), []) ], [])
    }
;;

let%test _ =
  succ_ev
    ( "say_meow"
    , Group [ Pipe (false, Simple ([], [ Word "echo"; Word "meow" ], []), []) ]
    , [ RedirInp (0, Word "inp.txt") ] )
    { empty_env with
      funs =
        SMap.singleton
          "say_meow"
          ( Group [ Pipe (false, Simple ([], [ Word "echo"; Word "meow" ], []), []) ]
          , [ RedirInp (0, Word "inp.txt") ] )
    }
;;

(* -------------------- Script -------------------- *)

open TestMake (struct
  type giv_t = script
  type exp_t = environment

  let pp_giv = pp_script
  let pp_res = pp_environment
  let ev = ev_script
  let cmp = cmp_envs
end)

let%test _ = succ_ev [] empty_env

let%test _ =
  match
    Parser.parse_result
      {|
f () if ((ABC == 0)); then echo yes && ABC=1 && f; else echo no; fi
f
|}
  with
  | Ok ast ->
    succ_ev
      ast
      { empty_env with
        funs =
          SMap.singleton
            "f"
            ( If
                ( Pipe
                    ( false
                    , Compound (ArithmExpr (Equal (Var ("ABC", "0"), Num 0)), [])
                    , [] )
                , PipeAndList
                    ( (false, Simple ([], [ Word "echo"; Word "yes" ], []), [])
                    , PipeAndList
                        ( ( false
                          , Simple ([ SimpleAssignt (("ABC", "0"), Word "1") ], [], [])
                          , [] )
                        , Pipe (false, Simple ([], [ Word "f" ], []), []) ) )
                , Some (Pipe (false, Simple ([], [ Word "echo"; Word "no" ], []), [])) )
            , [] )
      }
      ~exp_stdout:"yes\nno\n"
  | Error _ -> false
;;

let%test _ =
  match Parser.parse_result {|
f () { echo $1 $2 }
f a b
|} with
  | Ok ast ->
    succ_ev
      ast
      { empty_env with
        funs =
          SMap.singleton
            "f"
            ( Group
                [ Pipe
                    ( false
                    , Simple
                        ( []
                        , [ Word "echo"
                          ; ParamExp (Param ("1", "0"))
                          ; ParamExp (Param ("2", "0"))
                          ]
                        , [] )
                    , [] )
                ]
            , [] )
      }
      ~exp_stdout:"a b\n"
  | Error _ -> false
;;

let%test _ =
  match Parser.parse_result {|
ABC=10

echo_ABC () { echo $ABC }

ABC=5 echo_ABC
|} with
  | Ok ast ->
    succ_ev
      ast
      { empty_env with
        vars = SMap.singleton "ABC" (IndArray (IMap.singleton 0 "10"))
      ; funs =
          SMap.singleton
            "echo_ABC"
            ( Group
                [ Pipe
                    ( false
                    , Simple ([], [ Word "echo"; ParamExp (Param ("ABC", "0")) ], [])
                    , [] )
                ]
            , [] )
      }
      ~exp_stdout:"5\n"
  | Error _ -> false
;;
