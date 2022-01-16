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
