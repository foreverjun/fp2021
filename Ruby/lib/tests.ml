open Ast
open Parser

(*              *)
(* Parser Tests *)
(*              *)
let%test _ =
  parse p_final {|
1
-1
1.0
-1.0
true
"bread"
false
nil
|}
  = Ok
      [
        Expression (Constant (Integer 1));
        Expression (Constant (Integer (-1)));
        Expression (Constant (Float 1.));
        Expression (Constant (Float (-1.)));
        Expression (Constant (Boolean true));
        Expression (Constant (String "bread"));
        Expression (Constant (Boolean false));
        Expression Nil;
      ]

let%test _ =
  parse p_final
    {|
iam_local_var
@iam_instance_var
@@iam_class_var
$iam_global_var
|}
  = Ok
      [
        Expression (Variable (Local, Identifier "iam_local_var"));
        Expression (Variable (Instance, Identifier "iam_instance_var"));
        Expression (Variable (Class, Identifier "iam_class_var"));
        Expression (Variable (Global, Identifier "iam_global_var"));
      ]

let%test _ =
  parse p_final
    {|
1 + 1
1 + 1 + 1
1 + (1 + 1)
1 - 1
1 - 1 - 1
1 - (1 - 1)
1 * 1
1 * 1 * 1
1 * (1 * 1)
1 / 1
1 / 1 / 1
1 / (1 / 1)
1 % 1
1 % 1 % 1
1 % (1 % 1)
|}
  = Ok
      [
        Expression (Add (Constant (Integer 1), Constant (Integer 1)));
        Expression
          (Add
             ( Add (Constant (Integer 1), Constant (Integer 1)),
               Constant (Integer 1) ));
        Expression
          (Add
             ( Constant (Integer 1),
               Add (Constant (Integer 1), Constant (Integer 1)) ));
        Expression (Sub (Constant (Integer 1), Constant (Integer 1)));
        Expression
          (Sub
             ( Sub (Constant (Integer 1), Constant (Integer 1)),
               Constant (Integer 1) ));
        Expression
          (Sub
             ( Constant (Integer 1),
               Sub (Constant (Integer 1), Constant (Integer 1)) ));
        Expression (Mul (Constant (Integer 1), Constant (Integer 1)));
        Expression
          (Mul
             ( Mul (Constant (Integer 1), Constant (Integer 1)),
               Constant (Integer 1) ));
        Expression
          (Mul
             ( Constant (Integer 1),
               Mul (Constant (Integer 1), Constant (Integer 1)) ));
        Expression (Div (Constant (Integer 1), Constant (Integer 1)));
        Expression
          (Div
             ( Div (Constant (Integer 1), Constant (Integer 1)),
               Constant (Integer 1) ));
        Expression
          (Div
             ( Constant (Integer 1),
               Div (Constant (Integer 1), Constant (Integer 1)) ));
        Expression (Mod (Constant (Integer 1), Constant (Integer 1)));
        Expression
          (Mod
             ( Mod (Constant (Integer 1), Constant (Integer 1)),
               Constant (Integer 1) ));
        Expression
          (Mod
             ( Constant (Integer 1),
               Mod (Constant (Integer 1), Constant (Integer 1)) ));
      ]

let%test _ =
  parse p_final {|
if true
  x = 0
end

if false
  x = 0
else
  x = 1
end
|}
  = Ok
      [
        IfElse
          ( Constant (Boolean true),
            [ Assign (Variable (Local, Identifier "x"), Constant (Integer 0)) ],
            [] );
        IfElse
          ( Constant (Boolean false),
            [ Assign (Variable (Local, Identifier "x"), Constant (Integer 0)) ],
            [ Assign (Variable (Local, Identifier "x"), Constant (Integer 1)) ]
          );
      ]

let%test _ =
  parse p_final {| lambda{|x, y, z| x + y + z } |}
  = Ok
      [
        Expression
          (Lambda
             ( [
                 Variable (Local, Identifier "x");
                 Variable (Local, Identifier "y");
                 Variable (Local, Identifier "z");
               ],
               [
                 Expression
                   (Add
                      ( Add
                          ( Variable (Local, Identifier "x"),
                            Variable (Local, Identifier "y") ),
                        Variable (Local, Identifier "z") ));
               ] ));
      ]

let%test _ =
  parse p_final
    {|
x = 1
x = true
x = f()
x = lambda{|x| x }
x = 1 + 2 * 3 / (4 + 5 + 6) * 7 / 8
x = [1, 2, 3]
y = [f(), x()]
z = @x + @@y * $g
|}
  = Ok
      [
        Assign (Variable (Local, Identifier "x"), Constant (Integer 1));
        Assign (Variable (Local, Identifier "x"), Constant (Boolean true));
        Assign
          (Variable (Local, Identifier "x"), Call (Null, Identifier "f", []));
        Assign
          ( Variable (Local, Identifier "x"),
            Lambda
              ( [ Variable (Local, Identifier "x") ],
                [ Expression (Variable (Local, Identifier "x")) ] ) );
        Assign
          ( Variable (Local, Identifier "x"),
            Add
              ( Constant (Integer 1),
                Div
                  ( Mul
                      ( Div
                          ( Mul (Constant (Integer 2), Constant (Integer 3)),
                            Add
                              ( Add (Constant (Integer 4), Constant (Integer 5)),
                                Constant (Integer 6) ) ),
                        Constant (Integer 7) ),
                    Constant (Integer 8) ) ) );
        Assign
          ( Variable (Local, Identifier "x"),
            List
              [
                Constant (Integer 1); Constant (Integer 2); Constant (Integer 3);
              ] );
        Assign
          ( Variable (Local, Identifier "y"),
            List
              [
                Call (Null, Identifier "f", []); Call (Null, Identifier "x", []);
              ] );
        Assign
          ( Variable (Local, Identifier "z"),
            Add
              ( Variable (Instance, Identifier "x"),
                Mul
                  ( Variable (Class, Identifier "y"),
                    Variable (Global, Identifier "g") ) ) );
      ]

let%test _ =
  parse p_final
    {|
def func1
end

def func2
  x = 1
  return
end

def func3
  x = 1
  return x + y + 1
end
|}
  = Ok
      [
        Function (Identifier "func1", [], []);
        Function
          ( Identifier "func2",
            [],
            [
              Assign (Variable (Local, Identifier "x"), Constant (Integer 1));
              Return Nil;
            ] );
        Function
          ( Identifier "func3",
            [],
            [
              Assign (Variable (Local, Identifier "x"), Constant (Integer 1));
              Return
                (Add
                   ( Add
                       ( Variable (Local, Identifier "x"),
                         Variable (Local, Identifier "y") ),
                     Constant (Integer 1) ));
            ] );
      ]

let%test _ =
  parse p_final
    {|
class a1
end

class a2
  @@x = 1
end

class vehicle
  def start(a)
  end
end
|}
  = Ok
      [
        Class (Identifier "a1", []);
        Class
          ( Identifier "a2",
            [ Assign (Variable (Class, Identifier "x"), Constant (Integer 1)) ]
          );
        Class
          ( Identifier "vehicle",
            [
              Function
                (Identifier "start", [ Variable (Local, Identifier "a") ], []);
            ] );
      ]

let%test _ =
  parse p_final
    {|
while true
end

while x == true
  break
end

while x >= 1
  next
end
|}
  = Ok
      [
        While (Constant (Boolean true), []);
        While
          ( Equal (Variable (Local, Identifier "x"), Constant (Boolean true)),
            [ Break ] );
        While
          ( GreaterOrEq (Variable (Local, Identifier "x"), Constant (Integer 1)),
            [ Next ] );
      ]

let%test _ =
  parse p_final {|
f()
f(a, b, c)
f(1, 1.5, -6.8)
item.yeet(true)
|}
  = Ok
      [
        Expression (Call (Null, Identifier "f", []));
        Expression
          (Call
             ( Null,
               Identifier "f",
               [
                 Variable (Local, Identifier "a");
                 Variable (Local, Identifier "b");
                 Variable (Local, Identifier "c");
               ] ));
        Expression
          (Call
             ( Null,
               Identifier "f",
               [
                 Constant (Integer 1);
                 Constant (Float 1.5);
                 Constant (Float (-6.8));
               ] ));
        Expression
          (Call
             (Identifier "item", Identifier "yeet", [ Constant (Boolean true) ]));
      ]

let%test _ =
  parse p_final {|
1 == 1
1 > 0
0 < 1
1 >= 1
2 <= 3
|}
  = Ok
      [
        Expression (Equal (Constant (Integer 1), Constant (Integer 1)));
        Expression (Greater (Constant (Integer 1), Constant (Integer 0)));
        Expression (Less (Constant (Integer 0), Constant (Integer 1)));
        Expression (GreaterOrEq (Constant (Integer 1), Constant (Integer 1)));
        Expression (LessOrEq (Constant (Integer 2), Constant (Integer 3)));
      ]

let%test _ = parse p_final {||} = Ok []
let%test _ = parse p_final {||} = Ok []
let%test _ = parse p_final {||} = Ok []
let%test _ = parse p_final {||} = Ok []
let%test _ = parse p_final {||} = Ok []
