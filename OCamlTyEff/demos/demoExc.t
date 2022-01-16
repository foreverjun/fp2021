  $ cat << EOF | ./demo.exe -
  > let exc = raise1 ()
  > ;;
  > EOF
  Error: Exception: Exc1

  $ cat << EOF | ./demo.exe -
  > let exc =
  >   try raise1 () with
  >   | Exc1 -> 1
  > ;;
  > EOF
  val exc : int = 1

  $ cat << EOF | ./demo.exe -
  > let exc =
  >   try raise1 () with
  >   | Exc2 -> 2
  > ;;
  > EOF
  Error: Exception: Exc1

  $ cat << EOF | ./demo.exe -
  > let exc =
  >   try raise1 () with
  >   | Exc1 -> 1
  >   | Exc2 -> 2
  > ;;
  > EOF
  val exc : int = 1
