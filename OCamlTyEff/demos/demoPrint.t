  $ cat << EOF | ./demo.exe -
  > let rec iter = fun f -> fun xs ->
  >   match xs with
  >   | [] -> ()
  >   | x :: xs ->
  >     (match f x with
  >     | () -> iter f xs)
  > ;;
  > 
  > let o = iter println ("1" :: "2" :: "3" :: [])
  > ;;
  > EOF
  1
  2
  3
  val iter : (('t0 -['e0]-> unit) --> ('t0 list -['e0]-> unit)) = <fun>
  val o : unit = ()
