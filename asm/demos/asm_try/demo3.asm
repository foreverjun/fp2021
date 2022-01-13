
 global _start  ; необходимо для данного линкера

; скалярное произведение
;------------------------------
section .text

_start:

   mov rbx, 0x010203040506 ; вектор с элементами 1 2 3 4 5 6
   mov rcx, 0x060504030201 ; вектор с элементами 6 5 4 3 2 1
   mov rdx, 0xff
   mov rax, 0xff 
   
@b: and rdx, rbx
   and rax, rcx 
   imul rdx, rax 
   add rbp,rdx 
   shr rbx, 8
   shr rcx, 8
   mov rdx, 0xff
   mov rax, 0xff 
   cmp rbx, 0
jne @b
;------------------------------

; print number to ascii 
; 56 -> 0x38

section .data
codes db '0123456789ABCDEF'

section .text
mov rax, rbp
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
mov     rAx, 60
xor     rdi, rdi
syscall
