
 global _start  ; необходимо для данного линкера

; скалярное произведение
;------------------------------
section .data
 A db 1,2,3,4,5,6 ; вектор 1
B db 6,5,4,3,2,1  ; вектор 2
section .text

_start:

mov rbx, A ; A
   mov rcx, B ; B
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
; 56 -> '8'
mov rax, 1
  mov rdi, 1
  mov rsi, rbp ; результат в регистре rbp = 56
  mov rdx, 1
  syscall

; exit
mov     rAx, 60
xor     rdi, rdi
syscall
