'e3 effect type variable in fix type isn't particularly useful,
but type inferencer isn't smart enough to understand that it can 
safely be replaced with an empty set of effects
  $ cat << EOF | ./demo.exe -
  > let rec fix = 
  >   fun f -> fun eta -> f (fix f) eta
  > ;;
  > 
  > let print_list = fun self -> fun l ->
  >   match l with
  >   | hd :: tl ->
  >     let o = println hd in
  >     self tl
  >   | o -> ()
  > ;;
  > 
  > let fixed_print_list = fix print_list
  > 
  > let fac = fun self -> fun n -> 
  >   match n <= 1 with
  >   | true -> 1
  >   | false -> n * (self (n - 1))
  > ;;
  > 
  > let fixed_fac = fix fac
  > ;;
  > 
  > let n = fixed_fac 6
  > ;;
  > EOF
  val fix : ((('t0 -['e0, 'e1, 'e2, 'e3]-> 't1) -['e0]-> ('t0 -['e1]-> 't1)) -['e3]-> ('t0 -['e0, 'e1, 'e2, 'e3]-> 't1)) = <fun>
  val print_list : ((string list -['e0]-> unit) --> (string list -[IO, 'e0]-> unit)) = <fun>
  val fixed_print_list : (string list -[IO]-> unit) = <fun>
  val fac : ((int -['e0]-> int) --> (int -['e0]-> int)) = <fun>
  val fixed_fac : (int --> int) = <fun>
  val n : int = 720
