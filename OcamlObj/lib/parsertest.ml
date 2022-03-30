open Ast
open Parser

let test_parse ~label ~code ~expected =
  match parse prog code with
  | Error _ -> false
  | Result.Ok res when expected = res -> true
  | Result.Ok res ->
    let () =
      Printf.printf "[Parser test] %s failed.\nActual is:\n%s\n" label (show_progr res)
    in
    false
;;

let%test _ =
  test_parse
    ~label:"eobject"
    ~code:
      {|



let s = object
  val v = 10

  method minus = v - 1
  method plus = v + 1
  method times = 
    let helper = fun a -> a*2 in
    helper
  
end;;


let test = s#minus;;

    |}
    ~expected:
      [(DLet
    (false, (PVar "s"),
     (EObj
        [(EVal ((PVar "v"), (EConst (CInt 10))));
          (EMeth ((PVar "minus"),
             (EBinopr (Sub, (EVar "v"), (EConst (CInt 1))))));
          (EMeth ((PVar "plus"),
             (EBinopr (Add, (EVar "v"), (EConst (CInt 1))))));
          (EMeth ((PVar "times"),
             (ELet (
                (false, (PVar "helper"),
                 (EFun ((PVar "a"),
                    (EBinopr (Mul, (EVar "a"), (EConst (CInt 2))))))),
                (EVar "helper")))
             ))
          ])));
  (DLet (false, (PVar "test"), (ECallM ("s", "minus"))))]
;;


let%test _ =
  test_parse
    ~label:"Non-tailrec List map"
    ~code:
      {|

    let rec map f l = match l with [] -> [] | hd :: tl -> f hd :: map f tl

    |}
    ~expected:
      [ DLet
          ( true
          , PVar "map"
          , EFun
              ( PVar "f"
              , EFun
                  ( PVar "l"
                  , EMatch
                      ( EVar "l"
                      , [ PNil, ENil
                        ; ( PCons (PVar "hd", PVar "tl")
                          , ECons
                              ( EApp (EVar "f", EVar "hd")
                              , EApp (EApp (EVar "map", EVar "f"), EVar "tl") ) )
                        ] ) ) ) )
      ]
;;

let%test _ =
  test_parse
    ~label:"Tailrec fold_left"
    ~code:
      {|

    let rec fold_left f init = function 
    | [] -> init 
    | hd :: tl -> fold_left f (f init hd) tl

    |}
    ~expected:
      [ DLet
          ( true
          , PVar "fold_left"
          , EFun
              ( PVar "f"
              , EFun
                  ( PVar "init"
                  , EFun
                      ( PVar "match"
                      , EMatch
                          ( EVar "match"
                          , [ PNil, EVar "init"
                            ; ( PCons (PVar "hd", PVar "tl")
                              , EApp
                                  ( EApp
                                      ( EApp (EVar "fold_left", EVar "f")
                                      , EApp (EApp (EVar "f", EVar "init"), EVar "hd") )
                                  , EVar "tl" ) )
                            ] ) ) ) ) )
      ]
;;

let%test _ =
  test_parse
    ~label:"Dirty arithmetic"
    ~code:{|

    let _ = 4 / (1 + 1) :: 2*(2 + (2 - 3)) / 5 :: f x

    |}
    ~expected:
      [ DLet
          ( false
          , PWild
          , ECons
              ( EBinopr (Div, EConst (CInt 4), EBinopr (Add, EConst (CInt 1), EConst (CInt 1)))
              , ECons
                  ( EBinopr
                      ( Mul
                      , EConst (CInt 2)
                      , EBinopr
                          ( Div
                          , EBinopr
                              ( Add
                              , EConst (CInt 2)
                              , EBinopr (Sub, EConst (CInt 2), EConst (CInt 3)) )
                          , EConst (CInt 5) ) )
                  , EApp (EVar "f", EVar "x") ) ) )
      ]
;;

let%test _ =
  test_parse
    ~label:"Nested let-experssions"
    ~code:
      {|

    let a = 
      let b = 
        let c = 
          let d x = x + 1 
        in d 
      in c
      in fun x -> b (x / 2)

    |}
    ~expected:
      [ DLet
          ( false
          , PVar "a"
          , ELet
              (  ( false
                  , PVar "b"
                  , ELet
                      (  ( false
                          , PVar "c"
                          , ELet
                              (  ( false
                                  , PVar "d"
                                  , EFun (PVar "x", EBinopr (Add, EVar "x", EConst (CInt 1)))
                                  )
                                
                              , EVar "d" ) )
                        
                      , EVar "c" ) )
                
              , EFun (PVar "x", EApp (EVar "b", EBinopr (Div, EVar "x", EConst (CInt 2)))) )
          )
      ]
;;

let%test _ =
  test_parse
    ~label:"Long whitespaces"
    ~code:
      {|

    let   rec
       fibonacci    n  =

      if n <= 1  then n else 

      fibonacci
      (n - 1)   +
            fibonacci (n - 2)

    |}
    ~expected:
      [ DLet
          ( true
          , PVar "fibonacci"
          , EFun
              ( PVar "n"
              , EIf
                  ( EBinopr (Leeq, EVar "n", EConst (CInt 1))
                  , EVar "n"
                  , EBinopr
                      ( Add
                      , EApp (EVar "fibonacci", EBinopr (Sub, EVar "n", EConst (CInt 1)))
                      , EApp (EVar "fibonacci", EBinopr (Sub, EVar "n", EConst (CInt 2))) ) )
              ) )
      ]
;;


let%test _ =
  test_parse
    ~label:"Pattern matching in declaration"
    ~code:
      {|

      let (((e, s))) = 2, 2;;

      let rest :: [] = (e, s) :: [];;


      let (t, u, p, (l, (e, s))) = 1, 2, 3, (3, rest);;

      |}
    ~expected:
      [ DLet
          ( false
          , PTuple [ PVar "e"; PVar "s" ]
          , ETuple [ EConst (CInt 2); EConst (CInt 2) ] )
      ; DLet
          (false, PCons (PVar "rest", PNil), ECons (ETuple [ EVar "e"; EVar "s" ], ENil))
      ; DLet
          ( false
          , PTuple
              [ PVar "t"
              ; PVar "u"
              ; PVar "p"
              ; PTuple [ PVar "l"; PTuple [ PVar "e"; PVar "s" ] ]
              ]
          , ETuple
              [ EConst (CInt 1)
              ; EConst (CInt 2)
              ; EConst (CInt 3)
              ; ETuple [ EConst (CInt 3); EVar "rest" ]
              ] )
      ]
;;

let%test _ =
  test_parse
    ~label:"Catamorphism"
    ~code:
      {|

    let rec cata f e xs = match xs with [] -> e | x :: xs -> f x (cata f e xs);;

    let isort xs =
    let rec insert x lst =
      match lst with
      | [] -> [x]
      | h :: xs -> if x < h then x :: h :: xs else h :: insert x xs in
    cata insert xs []

    |}
    ~expected:
      [ DLet
          ( true
          , PVar "cata"
          , EFun
              ( PVar "f"
              , EFun
                  ( PVar "e"
                  , EFun
                      ( PVar "xs"
                      , EMatch
                          ( EVar "xs"
                          , [ PNil, EVar "e"
                            ; ( PCons (PVar "x", PVar "xs")
                              , EApp
                                  ( EApp (EVar "f", EVar "x")
                                  , EApp
                                      ( EApp (EApp (EVar "cata", EVar "f"), EVar "e")
                                      , EVar "xs" ) ) )
                            ] ) ) ) ) )
      ; DLet
          ( false
          , PVar "isort"
          , EFun
              ( PVar "xs"
              , ELet
                  (  ( true
                      , PVar "insert"
                      , EFun
                          ( PVar "x"
                          , EFun
                              ( PVar "lst"
                              , EMatch
                                  ( EVar "lst"
                                  , [ PNil, ECons (EVar "x", ENil)
                                    ; ( PCons (PVar "h", PVar "xs")
                                      , EIf
                                          ( EBinopr (Less, EVar "x", EVar "h")
                                          , ECons (EVar "x", ECons (EVar "h", EVar "xs"))
                                          , ECons
                                              ( EVar "h"
                                              , EApp
                                                  ( EApp (EVar "insert", EVar "x")
                                                  , EVar "xs" ) ) ) )
                                    ] ) ) ) )
                    
                  , EApp (EApp (EApp (EVar "cata", EVar "insert"), EVar "xs"), ENil) ) )
          )
      ]
;;

let%test _ =
  test_parse
    ~label:"LCS"
    ~code:
      {| 

    let lcs xs ys =
    let rec helper = function
      | [], _ -> []
      | _, [] -> []
      | x :: xs, y :: ys -> 
      if x = y 
      then x :: helper (xs, ys)
      else 
          (let r1 = helper (x :: xs, ys) in
          let r2 = helper (xs, y :: ys) in
          if list_len r1 > list_len r2 then r1 else r2)
    in
    helper (xs, ys)


    |}
    ~expected:
      [(DLet
    (false, (PVar "lcs"),
     (EFun ((PVar "xs"),
        (EFun ((PVar "ys"),
           (ELet (
              (true, (PVar "helper"),
               (EFun ((PVar "match"),
                  (EMatch ((EVar "match"),
                     [((PTuple [PNil; PWild]), ENil);
                       ((PTuple [PWild; PNil]), ENil);
                       ((PTuple
                           [(PCons ((PVar "x"), (PVar "xs")));
                             (PCons ((PVar "y"), (PVar "ys")))]),
                        (EIf ((EBinopr (Eq, (EVar "x"), (EVar "y"))),
                           (ECons ((EVar "x"),
                              (EApp ((EVar "helper"),
                                 (ETuple [(EVar "xs"); (EVar "ys")])))
                              )),
                           (ELet (
                              (false, (PVar "r1"),
                               (EApp ((EVar "helper"),
                                  (ETuple
                                     [(ECons ((EVar "x"), (EVar "xs")));
                                       (EVar "ys")])
                                  ))),
                              (ELet (
                                 (false, (PVar "r2"),
                                  (EApp ((EVar "helper"),
                                     (ETuple
                                        [(EVar "xs");
                                          (ECons ((EVar "y"), (EVar "ys")))])
                                     ))),
                                 (EIf (
                                    (EBinopr (Gre,
                                       (EApp ((EVar "list_len"), (EVar "r1")
                                          )),
                                       (EApp ((EVar "list_len"), (EVar "r2")
                                          ))
                                       )),
                                    (EVar "r1"), (EVar "r2")))
                                 ))
                              ))
                           )))
                       ]
                     ))
                  ))),
              (EApp ((EVar "helper"), (ETuple [(EVar "xs"); (EVar "ys")])))))
           ))
        ))))
  ]
;;

let%test _ =
  test_parse
    ~label:"Paramorphism"
    ~code:
      {|

  let rec para f e xs =
      match xs with [] -> e | x :: xs -> f x (xs, para f e xs)
    ;;

    let isort lt xs =
      let insert3 x lst =
        para
          (fun h (tl, acc) -> if lt x h then x :: h :: tl else h :: acc)
          [x] lst in
      cata_ insert3 xs []



  |}
    ~expected:
      [ DLet
          ( true
          , PVar "para"
          , EFun
              ( PVar "f"
              , EFun
                  ( PVar "e"
                  , EFun
                      ( PVar "xs"
                      , EMatch
                          ( EVar "xs"
                          , [ PNil, EVar "e"
                            ; ( PCons (PVar "x", PVar "xs")
                              , EApp
                                  ( EApp (EVar "f", EVar "x")
                                  , ETuple
                                      [ EVar "xs"
                                      ; EApp
                                          ( EApp (EApp (EVar "para", EVar "f"), EVar "e")
                                          , EVar "xs" )
                                      ] ) )
                            ] ) ) ) ) )
      ; DLet
          ( false
          , PVar "isort"
          , EFun
              ( PVar "lt"
              , EFun
                  ( PVar "xs"
                  , ELet
                      (  ( false
                          , PVar "insert3"
                          , EFun
                              ( PVar "x"
                              , EFun
                                  ( PVar "lst"
                                  , EApp
                                      ( EApp
                                          ( EApp
                                              ( EVar "para"
                                              , EFun
                                                  ( PVar "h"
                                                  , EFun
                                                      ( PTuple [ PVar "tl"; PVar "acc" ]
                                                      , EIf
                                                          ( EApp
                                                              ( EApp (EVar "lt", EVar "x")
                                                              , EVar "h" )
                                                          , ECons
                                                              ( EVar "x"
                                                              , ECons (EVar "h", EVar "tl")
                                                              )
                                                          , ECons (EVar "h", EVar "acc")
                                                          ) ) ) )
                                          , ECons (EVar "x", ENil) )
                                      , EVar "lst" ) ) ) )
                        
                      , EApp (EApp (EApp (EVar "cata_", EVar "insert3"), EVar "xs"), ENil)
                      ) ) ) )
      ]
;;


let%test _ =
  test_parse
    ~label:"Arithm with whitespaces"
    ~code:{|

    let ar = 2 -(7 && false ) *     3 - (2 * ( 20 || 29))

    |}
    ~expected:
      [ DLet
          ( false
          , PVar "ar"
          , EBinopr
              ( Sub
              , EConst (CInt 2)
              , EBinopr
                  ( Sub
                  , EBinopr
                      ( Mul
                      , EBinopr (And, EConst (CInt 7), EConst (CBool false))
                      , EConst (CInt 3) )
                  , EBinopr
                      (Mul, EConst (CInt 2), EBinopr (Or, EConst (CInt 20), EConst (CInt 29)))
                  ) ) )
      ]
;;

let%test _ =
  test_parse
    ~label:"Lambdas"
    ~code:
      {|

    let rec list_to_n = function 1 -> 1 | n -> n :: list_to_n (n - 1);;

    let rec reduce f = function [] -> 1 | x :: xs -> f x (reduce f xs);;

    let fact n = reduce (fun x y -> x * y) (list_to_n n)

    |}
    ~expected:
      [ DLet
          ( true
          , PVar "list_to_n"
          , EFun
              ( PVar "match"
              , EMatch
                  ( EVar "match"
                  , [ PConst (CInt 1), EConst (CInt 1)
                    ; ( PVar "n"
                      , ECons
                          ( EVar "n"
                          , EApp (EVar "list_to_n", EBinopr (Sub, EVar "n", EConst (CInt 1)))
                          ) )
                    ] ) ) )
      ; DLet
          ( true
          , PVar "reduce"
          , EFun
              ( PVar "f"
              , EFun
                  ( PVar "match"
                  , EMatch
                      ( EVar "match"
                      , [ PNil, EConst (CInt 1)
                        ; ( PCons (PVar "x", PVar "xs")
                          , EApp
                              ( EApp (EVar "f", EVar "x")
                              , EApp (EApp (EVar "reduce", EVar "f"), EVar "xs") ) )
                        ] ) ) ) )
      ; DLet
          ( false
          , PVar "fact"
          , EFun
              ( PVar "n"
              , EApp
                  ( EApp
                      ( EVar "reduce"
                      , EFun (PVar "x", EFun (PVar "y", EBinopr (Mul, EVar "x", EVar "y"))) )
                  , EApp (EVar "list_to_n", EVar "n") ) ) )
      ]
;;


let%test _ =
  test_parse
    ~label:"Fixpoint"
    ~code:
      {|


     let rec fix f x = f (fix f) x
     ;;
    let fac f n = if n <= 1 then 1 else f (n - 1) * n;;
    let fact = fix fac;;
    let res = fact 10;;


    |}
    ~expected:
      [ DLet
          ( true
          , PVar "fix"
          , EFun
              ( PVar "f"
              , EFun
                  (PVar "x", EApp (EApp (EVar "f", EApp (EVar "fix", EVar "f")), EVar "x"))
              ) )
      ; DLet
          ( false
          , PVar "fac"
          , EFun
              ( PVar "f"
              , EFun
                  ( PVar "n"
                  , EIf
                      ( EBinopr (Leeq, EVar "n", EConst (CInt 1))
                      , EConst (CInt 1)
                      , EBinopr
                          ( Mul
                          , EApp (EVar "f", EBinopr (Sub, EVar "n", EConst (CInt 1)))
                          , EVar "n" ) ) ) ) )
      ; DLet (false, PVar "fact", EApp (EVar "fix", EVar "fac"))
      ; DLet (false, PVar "res", EApp (EVar "fact", EConst (CInt 10)))
      ]
;;