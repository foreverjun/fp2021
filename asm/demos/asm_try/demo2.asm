global _start   ; необходимо для данного линкера

; Факториал
; ---------------------------------------------------
_start: mov rax,1  ; В rax будет факториал, на это число мы будем умножать все остальные. 
mov rcx,5          ; В rcx число, факториал которого нужно найти. 
 
factCycle: imul rax, rcx  ; умножаем rax на rcx
dec rcx            ; вычитаем из rcx единицу
cmp rcx,1          ; проверяем не равен ли rcx единице
jne factCycle      ; JNE= JUMP if  NOT EQUAL - переход если неравно.  если rcx != 1 - переходим на следующую итерацию.

; ---------------------------------------------------
; convert and print the value in rax to hexadecimal
; 5! = 120 = 0x78

section .data
codes db '0123456789ABCDEF'

section .text
mov rdi, 1
mov rdx, 1
mov rcx, 64
.loop push rax
sub rcx, 4
sar rax, cl
and rax, 0xf

lea rsi, [codes + rax]
mov rax, 1
push rcx
syscall
pop rcx
pop rax

test rcx, rcx
jnz .loop

; exit

 mov     rax, 60  ; exit syscall
 xor     rdi, rdi ; ret code 0
 syscall 
