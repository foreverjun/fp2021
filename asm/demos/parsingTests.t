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
     [(Data []);
       (Data
          [(EquDir ((Id "@messae2"), (Const -17578L)));
            (Instr (None, (Arg2 ((Mnemonic "MOV"), (Reg "RAX"), (Const 1L)))))]);
       (Code
          [(Instr (None, (Arg2 ((Mnemonic "MOV"), (Reg "RAX"), (Const 1L)))));
            (Instr (None, (Arg2 ((Mnemonic "MOV"), (Reg "RDI"), (Reg "RAX")))));
            (Instr (None,
               (Arg2 ((Mnemonic "MOV"), (Reg "RSI"), (Label "message")))));
            (Instr (None, (Arg2 ((Mnemonic "MOV"), (Reg "RDX"), (Const 14L)))))
            ]);
       (Data
          [(DataDecl ((Some (Id "message")), "DB",
              [(Dup ((Add ((Const 6L), (Const 1L))), "2")); (Str "r");
                (Expr (Const 10L))]
              ))
            ]);
       (Code
          [(Instr (None, (Arg0 (Mnemonic "SYSCALL"))));
            (Instr (None, (Arg2 ((Mnemonic "MOV"), (Reg "RAX"), (Const 60L)))));
            (Instr (None, (Arg2 ((Mnemonic "XOR"), (Reg "RDI"), (Reg "RDI")))));
            (Instr (None, (Arg0 (Mnemonic "SYSCALL"))))])
       ])
