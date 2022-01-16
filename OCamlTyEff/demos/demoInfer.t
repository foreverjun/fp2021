  $ cat << EOF | ./demo.exe -
  > let id = fun x -> x
  > 
  > let rec map = fun f -> fun xs ->
  >   match xs with
  >   | [] -> []
  >   | x :: xs -> f x :: map f xs
  > ;;
  > 
  > let id_map = map id
  > let println_map = map println
  > ;;
  > EOF
  val id : ('t0 --> 't0) = <fun>
  val map : (('t0 -['e0]-> 't1) --> ('t0 list -['e0]-> 't1 list)) = <fun>
  val id_map : ('_weak_t0 list --> '_weak_t0 list) = <fun>
  val println_map : (string list -[IO]-> unit list) = <fun>
