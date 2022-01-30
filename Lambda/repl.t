
  $ ./REPL.exe
  $ ./REPL.exe -help
  $ ./REPL.exe -cbv - <<EOF
  > \f.x
  Result: Abs (f, Var (x))
  $ ./REPL.exe -no - <<EOF
  > (\x.\y.x)(\u.u)((\x. x x)(\x.x x))
  Result: Abs (f, Var (x))
