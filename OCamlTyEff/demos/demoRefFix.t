  $ cat << EOF | ./demo.exe -
  > let fix = fun f ->
  >   let r = ref (fun o -> raise1 ()) in
  >   let fixf = fun x -> f !r x in
  >   let o = r := fixf in
  >   !r
  > 
  > let print_list =
  >   fix (fun self -> fun l ->
  >       match l with
  >       | hd :: tl ->
  >         let o = println hd in
  >         self tl
  >       | o -> ())
  > 
  > let o = print_list ( "a" :: "b" :: "c" :: [] )
  > ;;
  a
  b
  c
  val fix : ((('t0 -[exc Exc1, 'e0]-> 't1) -[exc Exc1, 'e0]-> ('t0 -[exc Exc1, 'e0]-> 't1)) -[Asgmt]-> ('t0 -[exc Exc1, 'e0]-> 't1)) = <fun>
  val print_list : (string list -[IO, exc Exc1]-> unit) = <fun>
  val o : unit = ()
