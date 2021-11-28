let reg =
  [ (* 8 bits *)
    "AH"
  ; "AL"
  ; "BH"
  ; "BL"
  ; "CH"
  ; "CL"
  ; "DH"
  ; "DL"
  ; (* 16 bits *)
    "AX"
  ; "BX"
  ; "CX"
  ; "DX"
  ; (* General-purpose *)
    "EAX"
  ; "EBX"
  ; "ECX"
  ; "EDX"
  ; "ESI"
  ; "EDI"
  ; (* Stack pointer *)
    "ESP"
  ; (* Base pointer *)
    "EBP"
  ; (* 128 bits SSE *)
    "XMM0"
  ; "XMM1"
  ; "XMM2"
  ; "XMM3"
  ; "XMM4"
  ; "XMM5"
  ; "XMM6"
  ; "XMM7"
  ]
;;

let instruction =
  [ (* --- Movement --- *)
    "MOV"
    (* 
  mov <reg>,<reg>
  mov <reg>,<mem>
  mov <mem>,<reg>
  mov <reg>,<con>
  mov <mem>,<con> 
  *)
  ; "PUSH"
    (* 
  push <reg32>
  push <mem>
  push <con32> 
  *)
  ; "POP"
    (* 
  pop <reg32>
  pop <mem>
  *)
  ; "LEA"
    (* 
  lea <reg32>,<mem>
  *)
  ; (* --- Arithmetic and Logic --- *)
    "ADD"
  ; "SUB"
    (* 
  add/sub <reg>,<reg>
  add/sub <reg>,<mem>
  add/sub <mem>,<reg>
  add/sub <reg>,<con>
  add/sub <mem>,<con>
  *)
  ; "INC"
  ; "DEC"
    (* 
  inc/dec <reg>
  inc/dec <mem> 
  *)
  ; "IMUL"
    (* 
  imul <reg32>,<reg32>
  imul <reg32>,<mem>
  imul <reg32>,<reg32>,<con>
  imul <reg32>,<mem>,<con>
  *)
  ; "IDIV"
    (* 
  idiv <reg32>
  idiv <mem>
  *)
  ; "AND"
  ; "OR"
  ; "XOR"
    (* 
  and/or/xor <reg>,<reg>
  and/or/xor <reg>,<mem>
  and/or/xor <mem>,<reg>
  and/or/xor <reg>,<con>
  and/or/xor <mem>,<con>
  *)
  ; "NOT"
  ; "NEG"
    (* 
  not/neg <reg>
  not/neg <mem>
   *)
  ; "SHL"
  ; "SHR"
    (* 
  shl/shr <reg>,<con8>
  shl/shr <mem>,<con8>
  shl/shr <reg>,<cl>
  shl/shr <mem>,<cl>
  *)
  ; (* --- Control Flow --- *)
    "JMP"
    (* 
  jmp <label>
  *)
  ; "JE"
  ; "JNE"
  ; "JZ"
  ; "JG"
  ; "JGE"
  ; "JL"
  ; "JLE"
    (* 
  j* <label>
  *)
  ; "CMP"
    (* 
  cmp <reg>,<reg>
  cmp <reg>,<mem>
  cmp <mem>,<reg>
  cmp <reg>,<con>
  *)
  ; "CALL"
    (* 
  call <label>
  *)
  ; "RET"
    (* 
  ret
  *)
  ; (* --- SSE --- *)
    "MOVUPS"
  ; "MOVSS"
  ; "MOVLPS"
  ; "MOVHPS"
    (* 
  movups <regSSE>,<regSSE>
  movups <regSSE>,<mem>
  movups <regSSE>,<con> 
  movups <mem>,<regSSE>
  movups <mem>,<con> 
  *)
  ; "ADDSS"
  ; "MULSS"
  ; "SUBSS"
  ; "SHUFPS"
  ]
;;