  $ cat << EOF | ./demo.exe -
  > let fresh_id =
  >   let last_id = ref (-1) in
  >   fun o ->
  >     let o = last_id := !last_id + 1 in
  >     !last_id
  > ;;
  > 
  > let a = fresh_id ()
  > let b = fresh_id ()
  > let c = fresh_id ()
  > ;;
  > EOF
  val fresh_id : ('t0 -[Asgmt]-> int) = <fun>
  val a : int = 0
  val b : int = 1
  val c : int = 2
