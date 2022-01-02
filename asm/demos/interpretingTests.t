  $ ./demoAsm.exe <<-EOF
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
  > j inc rbX
  > cmp rbx, 8
  > JNE j
  >     mov     rdi,rAx
  >     mov     rsi, message
  >     mov     rdx,14
  > 
  > section .data
  > message: db 6+1 dup "2"   , "r", 0xA
  > v1 db "Oh hi"
  > v2 db "\x00\x00\x00\x00\x00, Mark!    "
  > section .text
  >     syscall
  >   
  > 
  >     movapd     Xmm2, v1
  >     movapd     Xmm1, v2
  >     mov     rAx, 60
  >     xor     rdi, rdi
  >     syscall
  > 
  > 
  > 
  > 
  > EOF
  ["0cond": (R64 0L),
   "0jump": (Ls ""),
   "0retcode": (R64 0L),
   "@messae2": (Ls "V\187\255\255\255\255\255\255"),
   "RAX": (R64 60L),
   "RBP": (R64 0L),
   "RBX": (R64 8L),
   "RCX": (R64 0L),
   "RDI": (R64 0L),
   "RDX": (R64 14L),
   "RSI": (R64 4369727920800031844L),
   "RSP": (R64 0L),
   "XMM0": (RSSE "\000"),
   "XMM1": (RSSE "\\x00\\x00\\x00\\x00\\x00, Mark!    "),
   "XMM2": (RSSE "Oh hi"),
   "XMM3": (RSSE "\000"),
   "XMM4": (RSSE "\000"),
   "XMM5": (RSSE "\000"),
   "XMM6": (RSSE "\000"),
   "XMM7": (RSSE "\000"),
   "message": (Ls "22222222222222r\n"),
   "v1": (Ls "Oh hi"),
   "v2": (Ls "\\x00\\x00\\x00\\x00\\x00, Mark!    "),
   ]
