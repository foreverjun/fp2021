  $ ./demoOcamlADT.exe <<-EOF
  >  type person = | Age of int | Name of string
  > 
  >  let misha = Age 2
  val misha = Age 2
  $$$$
  $ ./demoOcamlADT.exe <<-EOF
  >  let x y = y + 2
  >  let q w = w + 2
  >  let z = q (x 2)
  val x = <fun>
  val q = <fun>
  val z = 6
  $$$$
  $ ./demoOcamlADT.exe <<-EOF
  >  let x = 1 :: 2 :: []
  val x = [1; 2]
  $$$$
  $ ./demoOcamlADT.exe <<-EOF
  >  let x y = y + 2
  >  let z = x 2
  val x = <fun>
  val z = 4
  $$$$
  $ ./demoOcamlADT.exe <<-EOF
  >  let rec map f = function
  >  | [] -> []
  >  | h :: tl -> f h :: map f tl
  > 
  >  let x = [1;2;3]
  >  let y = map (fun x -> x + 3) [1;2;3]
  val map = <fun>
  val x = [1; 2; 3]
  val y = [4; 5; 6]
  $$$$
  $ ./demoOcamlADT.exe <<-EOF
  >  let rec fact x = if x <= 1 then 1 else x * fact (x - 1)
  >  let x = fact 5
  val fact = <fun>
  val x = 120
  $$$$
  $ ./demoOcamlADT.exe <<-EOF
  >  let rec fibonacci n =
  >  if n < 3 then
  >    1
  >  else
  >    fibonacci (n-1) + fibonacci (n-2)
  > 
  >  let x = fibonacci 10
  val fibonacci = <fun>
  val x = 55
  $$$$
  $ ./demoOcamlADT.exe <<-EOF
  >  type person = | Age of int | Name of string
  > 
  >  let misha_age = Age 19
  >  let misha_name = Name "Misha"
  > 
  >  let age = function | Age age -> age | Name _ -> 0
  >  let test1 = age misha_age
  >  let test2 = age misha_name
  >  let test3 = age (Age 20)
  >  let test4 = age (Name "Misha after 27 november")
  val misha_age = Age 19
  val misha_name = Name Misha
  val age = <fun>
  val test1 = 19
  val test2 = 0
  val test3 = 20
  val test4 = 0
  $$$$
(* Realization of RB tree was taken here:
https://github.com/CompScienceClub/ocaml-red-black-trees/blob/master/src/red_black_tree.ml *)
  $ ./demoOcamlADT.exe <<-EOF
  >  type color = | R | B | BB
  >  type tree = | Empty of color | Node of color * tree * int * tree
  > 
  >  let rec member x = function
  >    | Empty _ -> false
  >    | Node (_, l, q, r) ->
  >        if x = q then true else if q < x then member x r else member x l
  > 
  >  let bal_ins_l = function
  >    | Node (B, Node (R, Node (R, a, x, b), y, c), z, d) ->
  >        Node (R, Node (B, a, x, b), y, Node (B, c, z, d))
  >    | Node (B, Node (R, a, x, Node (R, b, y, c)), z, d) ->
  >        Node (R, Node (B, a, x, b), y, Node (B, c, z, d))
  >    | n -> n
  > 
  >  let bal_ins_r = function
  >    | Node (B, a, x, Node (R _, Node (R _, b, y, c), z, d)) ->
  >        Node (R, Node (B, a, x, b), y, Node (B, c, z, d))
  >    | Node (B _, a, x, Node (R, b, y, Node (R, c, z, d))) ->
  >        Node (R, Node (B, a, x, b), y, Node (B, c, z, d))
  >    | n -> n
  > 
  >  let ins x =
  >    let rec ins_int = function
  >      | Empty _ -> Node (R, Empty B, x, Empty B)
  >      | Node (c, l, q, r) ->
  >          if q < x then bal_ins_r (Node (c, l, q, ins_int r))
  >          else if q > x then bal_ins_l (Node (c, ins_int l, q, r))
  >          else Node (c, l, q, r) in
  >    ins_int
  > 
  >  let insert t x =
  >    match ins x t with
  >    | Empty _ -> Node (B, Empty B, 0, Empty B)
  >    | Node (_, q, l, r) -> Node (B, q, l, r)
  > 
  >  let root = insert (Empty B) 5
  >  let root = insert root 2
  >  let root = insert root 8
  >  let root = insert root 7
  > 
  >  let test1 = member 2 root
  >  let test2 = member 10 root
  val member = <fun>
  val bal_ins_l = <fun>
  val bal_ins_r = <fun>
  val ins = <fun>
  val insert = <fun>
  val root = Node (B (), Empty B (), 5, Empty B ())
  val root = Node (B (), Node (R (), Empty B (), 2, Empty B ()), 5, Empty B ())
  val root = Node (B (), Node (R (), Empty B (), 2, Empty B ()), 5, Node (R (), Empty B (), 8, Empty B ()))
  val root = Node (B (), Node (B (), Node (R (), Empty B (), 2, Empty B ()), 5, Empty B ()), 7, Node (B (), Empty B (), 8, Empty B ()))
  val test1 = true
  val test2 = false
  $$$$
  $ ./demoOcamlADT.exe <<-EOF
  > let rec fib n k = if n < 2 then n else fib (n-1) (fun l -> fib (n-2) (fun r -> k (l+r)))
  > let ans = fib 5 (fun x -> x)
  val fib = <fun>
  val ans = 1
  $$$$
