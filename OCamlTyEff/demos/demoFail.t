  $ cat << EOF | ./demo.exe -
  > let x = y
  > ;;
  > EOF
  Error: Undefined variable 'y'

  $ cat << EOF | ./demo.exe -
  > let n =
  >   match 1 :: [] with
  >   | [] -> 0
  >   | a :: b :: tl -> 1
  > ;;
  > ;;
  > EOF
  Error: This pattern-matching is not exhaustive:
  []
  a :: b :: tl

  $ cat << EOF | ./demo.exe -
  > let n = 1 / 0
  > ;;
  > EOF
  Error: Division by zero

  $ cat << EOF | ./demo.exe -
  > let eq = println = println
  > ;;
  > EOF
  Error: Compare: functional value

  $ cat << EOF | ./demo.exe -
  > let f = fun l -> l :: l
  > ;;
  > EOF
  Error: Occurs check failed

  $ cat << EOF | ./demo.exe -
  > let rec x = x
  > ;;
  > EOF
  Error: This kind of expression is not allowed as right-hand side of `let rec x`

  $ cat << EOF | ./demo.exe -
  > let f = fun x -> x && (x < 0)
  > ;;
  > EOF
  Error: Unification failed on bool and int

  $ cat << EOF | ./demo.exe -
  > let n =
  >  match 1, 1 with
  >  | x, x -> x
  > ;;
  > EOF
  Error: Variable 'x' is bound several times in the matching
