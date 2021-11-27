```
⮕ directiveList ⮕ 
      directive directiveList
      | directive 
      
directive ⮕ simpleSegDir ⟦ inSegDirList ⟧ 

simpleSegDir ⮕ SECTION segDir ;;  (изначально мы в секции текста.)

segDir ⮕
      .CODE | .TEXT
      | .DATA  
      | .CONST
      
inSegDir ⮕ ⟦ labelDef ⟧ inSegmentDir

inSegDirList ⮕
      inSegDir inSegDirList
      | inSegDir 
      
labelDef ⮕ id ⟦:⟧

inSegmentDir ⮕
      instruction  
      | dataDir  
      | generalDir
      
generalDir ⮕
      equDir 
      | =Dir
      
mnemonic ⮕ Имя инструкции.

instruction ⮕ mnemonic ⟦ exprList ⟧

exprList ⮕  
      expr , exprList
      | expr 
      
expr ⮕
      expr addOp e01 
      | e01   
      
e01 ⮕ 
      e01 mulOp e02 
      | e01 shiftOp e02 
      | e02 
      
e02 ⮕
      e02 addOp e03 
      | e03
      
e03 ⮕
      ( expr )
      | string  
      | constant 
      | id  
      | register
      | $
      
stext ⮕
      stringChar stext 
      | stringChar
      
string ⮕ quote ⟦ stext ⟧ quote

stringChar ⮕ 
      quote quote 
      | Любой символ, кроме кавычек.
      
constant ⮕ ⟦ sign ⟧  digits

digits ⮕
      0X⟦ hexdigits ⟧
      | decNumber

decNumber ⮕
      decdigit decNumber
      | decdigit
      
hexNumber ⮕
      hexdigit hexNumber
      | decdigit hexNumber
      | hexdigit
      | decdigit
      
dataDir ⮕ dataItem ;;

dataItem ⮕ dataDecl scalarInstList

dataDecl ⮕ 
      DB 
      | DW 
      | DD 
      | DF 
      | DQ 
      | DT
      
scalarInstList ⮕
      initValue , ⟦ ;; ⟧ scalarInstList 
      | initValue
      
initValue ⮕
      expr  
      | string  
      | ?  
      | expr DUP ( scalarInstList ) 

sign ⮕ + | -
      
equDir ⮕ textMacroId EQU expr ;;

textMacroId ⮕ id

=Dir ⮕ id = expr ;;

comment ⮕ ; text ;;

character ⮕ Любой символ с порядковым номером в диапазоне от 0 до 255, за исключением перевода строки (10). 

text ⮕ 
      character text 
      | character
      
whiteSpaceCharacter ⮕ ASCII 8, 9, 11 – 13, 26, 32

hexdigit ⮕ A | B | C | D | E | F 

decdigit ⮕ 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

alpha ⮕ Любая прописная или строчная буква (A-Z) или один из следующих четырех символов: @ _ $ ?

id ⮕ 
      decdigit id  
      | alpha id  
      | alpha
      
quote ⮕ " | ' 

shiftOp ⮕ >> | <<

mulOp ⮕ * | / | %

addOp ⮕ + | -

;; ⮕ endOfLine | comment 

byteRegister ⮕ AL | AH | BL | BH | CL | CH | DL | DH 

SIMDRegister ⮕ XMM0 | XMM1 | XMM2 | XMM3 | XMM4 | XMM5 | XMM6 | XMM7

gpRegister ⮕ AX | EAX | CX | ECX | DX | EDX | BX | EBX | DI | EDI | SI | ESI | BP | EBP | SP | ESP

qwordRegister ⮕ RAX | RCX | RDX | RBX | RSP | RBP | RSI | RDI

register ⮕ gpRegister | byteRegister | qwordRegister | SIMDRegister
 ``` 
 
