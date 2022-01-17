1 Stress test
checks multiple functions
  $ ./demoAsm.exe <<-EOF
  > 
  > section .data
  > section .data
  > @messae2:equ - 0x44aA
  > mov rax,1
  > section .text
  > mov     rax,0x001
  > j inc rbX
  > cmp rbx, 8
  > JNE j
  > mov     rdi,rAx
  > mov     rsi, message
  > mov     rdx,14
  > 
  > section .data
  > message: db 6+1 dup "2"   , "r", 0xA
  > v1 db "Oh hi"
  > v2 db "     , Mark!"
  > section .text
  > syscall
  >   
  > movapd     Xmm2, v1
  > movapd     Xmm3, v2
  > addpd     xmm2, xmm1
  > mov     rAx, 60
  > xor     rdi, rdi
  > syscall
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
   "XMM1": (RSSE "\000"),
   "XMM2": (RSSE "Oh hi\000\000\000\000\000\000\000\000\000\000\000"),
   "XMM3": (RSSE "     , Mark!"),
   "XMM4": (RSSE "\000"),
   "XMM5": (RSSE "\000"),
   "XMM6": (RSSE "\000"),
   "XMM7": (RSSE "\000"),
   "message": (Ls "22222222222222r\n"),
   "v1": (Ls "Oh hi"),
   "v2": (Ls "     , Mark!"),
   ]

2 Factorial
  $ ./demoAsm.exe <<-EOF
  > ; -----------------------------------------------------
  > _start: mov rbx,1  ; В rbx будет факториал, на это число мы будем умножать все остальные. 
  > mov rcx,5          ; В rcx число факториал которого нужно найти. 
  > 
  > factCycle: imul rbx, rcx  ; умножаем rbx на rcx
  > dec rcx            ; вычитаем из rcx единицу
  > cmp rcx,1          ; проверяем не равен ли rcx единице
  > jne factCycle      ; JNE= JUMP if  NOT EQUAL - переход если неравно.  если rcx != 1 - переходим на следующую итерацию.
  > 
  > ; -----------------------------------------------------
  > 
  > mov rax, 60  ; exit syscall
  > xor rdi, rdi ; ret code 0
  > syscall 
  > EOF
  ["0cond": (R64 0L),
   "0jump": (Ls ""),
   "0retcode": (R64 0L),
   "RAX": (R64 60L),
   "RBP": (R64 0L),
   "RBX": (R64 120L),
   "RCX": (R64 1L),
   "RDI": (R64 0L),
   "RDX": (R64 0L),
   "RSI": (R64 0L),
   "RSP": (R64 0L),
   "XMM0": (RSSE "\000"),
   "XMM1": (RSSE "\000"),
   "XMM2": (RSSE "\000"),
   "XMM3": (RSSE "\000"),
   "XMM4": (RSSE "\000"),
   "XMM5": (RSSE "\000"),
   "XMM6": (RSSE "\000"),
   "XMM7": (RSSE "\000"),
   ]


3 Скалярное произведение
  $ ./demoAsm.exe <<-EOF
  > ;--------------------------------------
  > section .data
  > A db 1,2,3,4,5,6 ; вектор 1
  > B db 6,5,4,3,2,1 ; вектор 2
  > section .text
  > 
  > _start: xor rbp, rbp ; результат в регистре rbp
  > mov rbx, A ; A
  > mov rcx, B ; B
  > mov rdx, 0xff
  > mov rax, 0xff 
  > 
  > @b: and rdx, rbx
  > and rax, rcx 
  > imul rdx, rax 
  > add rbp,rdx 
  > shr rbx, 8
  > shr rcx, 8
  > mov rdx, 0xff
  > mov rax, 0xff 
  > cmp rbx, 0
  > jne @b
  >  ; ------------------------------------
  > mov rax, 60  ; exit syscall
  > xor rdi, rdi ; ret code 0
  > syscall 
  > 
  > EOF
  ["0cond": (R64 0L),
   "0jump": (Ls ""),
   "0retcode": (R64 0L),
   "A": (Ls "\001\002\003\004\005\006"),
   "B": (Ls "\006\005\004\003\002\001"),
   "RAX": (R64 60L),
   "RBP": (R64 56L),
   "RBX": (R64 0L),
   "RCX": (R64 0L),
   "RDI": (R64 0L),
   "RDX": (R64 255L),
   "RSI": (R64 0L),
   "RSP": (R64 0L),
   "XMM0": (RSSE "\000"),
   "XMM1": (RSSE "\000"),
   "XMM2": (RSSE "\000"),
   "XMM3": (RSSE "\000"),
   "XMM4": (RSSE "\000"),
   "XMM5": (RSSE "\000"),
   "XMM6": (RSSE "\000"),
   "XMM7": (RSSE "\000"),
   ]

4 Произведение матриц
A: [1 2]  B: [6 4]
.  [3 4]     [5 3]
AxB: [16 10] =hex [10 0A]
.    [38 24]      [26 18]
=> 100A2618 => в RBP должно быть 269100568
  $ ./demoAsm.exe <<-EOF
  > ;--------------------------------------
  > section .data
  > A db 1,2,3,4 ; матрица 1
  > B db 6,5,4,3 ; матрица 2 (T)
  > n equ 2
  > section .text
  > _start: mov rsi, n
  > push rax
  > 
  > loop1: cmp rsi, 0
  > je end
  > mov rsp, n
  > dec rsi
  >  
  > loop2: cmp rsp, 0
  > je loop1
  > mov rbx, A
  > shr rbx, 16 * (n - rsi - 1)
  > and rbx, 0xffff
  > mov rcx, B
  > shr rcx, 16 * (n - rsp)
  > dec rsp
  > and rcx, 0xffff
  > 
  > xor rbp, rbp ; результат в регистре rbp
  > mov rdx, 0xff
  > mov rax, 0xff 
  > 
  > @b: and rdx, rbx
  > and rax, rcx 
  > imul rdx, rax 
  > add rbp,rdx 
  > shr rbx, 8
  > shr rcx, 8
  > mov rdx, 0xff
  > mov rax, 0xff 
  > cmp rbx, 0
  > jne @b
  > 
  > pop rdi
  > shl rdi, 8
  > or rdi, rbp
  > push rdi
  > 
  > jmp loop2
  >  ; ------------------------------------
  > end: pop rbp
  > mov rax, 60  ; exit syscall
  > xor rdi, rdi ; ret code 0
  > syscall 
  > 
  > EOF
  ["0cond": (R64 0L),
   "0jump": (Ls ""),
   "0retcode": (R64 0L),
   "A": (Ls "\001\002\003\004"),
   "B": (Ls "\006\005\004\003"),
   "RAX": (R64 60L),
   "RBP": (R64 269100568L),
   "RBX": (R64 0L),
   "RCX": (R64 0L),
   "RDI": (R64 0L),
   "RDX": (R64 255L),
   "RSI": (R64 0L),
   "RSP": (R64 0L),
   "XMM0": (RSSE "\000"),
   "XMM1": (RSSE "\000"),
   "XMM2": (RSSE "\000"),
   "XMM3": (RSSE "\000"),
   "XMM4": (RSSE "\000"),
   "XMM5": (RSSE "\000"),
   "XMM6": (RSSE "\000"),
   "XMM7": (RSSE "\000"),
   "n": (Ls "\002"),
   ]


5 XMM register
перемещение данных в регистры и сложение их.
сумма в XMM2 регистре.
  $ ./demoAsm.exe <<-EOF
  > section .data
  > v1 db '!3(45) 196'
  > v2 db ".57446-095" 
  > section .text
  > movapd     Xmm2, v1
  > movapd     Xmm3, v2
  > addpd     xmm2, xmm3
  > mov     rAx, 60
  > xor     rdi, rdi
  > syscall
  > 
  > EOF
  ["0cond": (R64 0L),
   "0jump": (Ls ""),
   "0retcode": (R64 0L),
   "RAX": (R64 60L),
   "RBP": (R64 0L),
   "RBX": (R64 0L),
   "RCX": (R64 0L),
   "RDI": (R64 0L),
   "RDX": (R64 0L),
   "RSI": (R64 0L),
   "RSP": (R64 0L),
   "XMM0": (RSSE "\000"),
   "XMM1": (RSSE "\000"),
   "XMM2": (RSSE "Oh_hi_Mark\000\000\000\000\000\000"),
   "XMM3": (RSSE ".57446-095"),
   "XMM4": (RSSE "\000"),
   "XMM5": (RSSE "\000"),
   "XMM6": (RSSE "\000"),
   "XMM7": (RSSE "\000"),
   "v1": (Ls "!3(45) 196"),
   "v2": (Ls ".57446-095"),
   ]

