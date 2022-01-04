Tests about parsing go here. It's expected that programs parse something and
output a parse tree.
For example, where you test correctness of AST it's recommended to put both
input and output into this file. In this case it will be easier to check that
the answer is correct.

  $ ./demoParse.exe <<-"EOF"
  > VAR=100
  > 
  > function some_f () {
  >   echo something
  > } >> output.txt
  > 
  > for i in 1 2 3; do
  >   echo $i
  > done | grep 2
  > 
  > echo 1 && echo 2 || echo 3
  > EOF
  [(Pipes
      (Pipe
         (false,
          (Simple ([(SimpleAssignt (("VAR", "0"), (Word "100")))], [], [])), 
          [])));
    (Func
       ("some_f",
        (Group
           [(Pipe
               (false, (Simple ([], [(Word "echo"); (Word "something")], [])),
                []))
             ]),
        [(AppendOtp (1, (Word "output.txt")))]));
    (Pipes
       (Pipe
          (false,
           (Compound (
              (ForList ("i", [(Word "1"); (Word "2"); (Word "3")],
                 (Pipe
                    (false,
                     (Simple ([],
                        [(Word "echo"); (ParamExp (Param ("i", "0")))], 
                        [])),
                     []))
                 )),
              [])),
           [(Simple ([], [(Word "grep"); (Word "2")], []))])));
    (Pipes
       (PipeAndList (
          (false, (Simple ([], [(Word "echo"); (Word "1")], [])), []),
          (PipeOrList (
             (false, (Simple ([], [(Word "echo"); (Word "2")], [])), []),
             (Pipe (false, (Simple ([], [(Word "echo"); (Word "3")], [])), []))
             ))
          )))
    ]
