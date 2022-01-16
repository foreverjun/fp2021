  $ cat << EOF | ./demo.exe -
  > let rec map2 = fun f -> fun l1 -> fun l2 ->
  >   match l1, l2 with
  >   | [], [] -> []
  >   | a1 :: l1, a2 :: l2 ->
  >     let r = f a1 a2 in
  >     r :: map2 f l1 l2
  >   | o1, o2 -> raise1 ()
  > ;;
  > 
  > let l = map2 (fun x -> fun y -> x, y) ( 1 :: 2 :: [] ) ( "1" :: "2" :: [] )
  > ;;
  > EOF
  val map2 : (('t0 -['e0]-> ('t1 -['e1]-> 't2)) --> ('t0 list --> ('t1 list -[exc Exc1, 'e0, 'e1]-> 't2 list))) = <fun>
  val l : (int * string) list = [(1, "1"); (2, "2")]
