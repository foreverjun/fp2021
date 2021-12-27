Tests about parsing go here. It's expected that programs parse something and
output a parse tree.
For example, where your test correctness of AST it's recommend to put both
input and output into this file. In this case it will be easier to check that
answer is correct

  $ ./demoParse.exe <<-EOF
  > 
  > section .data
  > section .data
  > @messae2:equ - 0x44aA
  > mov     rax,1
  > 
  > 
  > section .text
  > _: 
  > 
  > mov     rax,0x001
  >     mov     rdi,rax
  >     mov     rsi, message
  >     mov     rdx,14
  > 
  > section .data
  > message: db 6+1 dup "2"   , "r", 0xA
  > section .text
  >     syscall
  > 
  >     mov     rAx, 60
  >     xor     rdi, rdi
  >     syscall
  > 
  > 
  > 
  > 
  > EOF
  (Directive
     [(Section ("DATA", []));
       (Section ("DATA",
          [(EquDir ((Id "@messae2"), (Const -17578)));
            (InDir (None,
               (Instruction ((Mnemonic "MOV"), [(Reg "RAX"); (Const 1)]))))
            ]
          ));
       (Section ("TEXT",
          [(InDir ((Some (Id "_")),
              (Instruction ((Mnemonic "MOV"), [(Reg "RAX"); (Const 1)]))));
            (InDir (None,
               (Instruction ((Mnemonic "MOV"), [(Reg "RDI"); (Reg "RAX")]))));
            (InDir (None,
               (Instruction ((Mnemonic "MOV"), [(Reg "RSI"); (Label "message")]
                  ))
               ));
            (InDir (None,
               (Instruction ((Mnemonic "MOV"), [(Reg "RDX"); (Const 14)]))))
            ]
          ));
       (Section ("DATA",
          [(InDir ((Some (Id "message")),
              (DataDecl ("DB",
                 [(Dup ((Add ((Const 6), (Const 1))), "2")); (Str "r");
                   (Expr (Const 10))]
                 ))
              ))
            ]
          ));
       (Section ("TEXT",
          [(InDir (None, (Instruction ((Mnemonic "SYSCALL"), []))));
            (InDir (None,
               (Instruction ((Mnemonic "MOV"), [(Reg "RAX"); (Const 60)]))));
            (InDir (None,
               (Instruction ((Mnemonic "XOR"), [(Reg "RDI"); (Reg "RDI")]))));
            (InDir (None, (Instruction ((Mnemonic "SYSCALL"), []))))]
          ))
       ])
