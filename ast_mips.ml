(* Syntaxe abstraite pour le langage MIPS *)

type stmt =
  | Sbinopi of binopi*registre*registre*expr
  | Sbinop of binop*registre*registre*registre 
  | Smonopi of monopi*registre*expr
  | Smonop of monop*registre*registre
  | Sjump of jump
  | Slabel of string
  | Ssyscall
  | Scond of cond2*registre*registre
and cond =
  |Beq
  |Bne
  |Blt
  |Bge
and expr =
  | Intm of int

and registre =
  |Zero |At |V0 |V1 |A of int |T of int |S of int |K0 |K1 |Gp |Sp |Fp |Ra |Hi |Lo |Pc

and jump =
  | J of string
  | Jr of registre
  | Jal of string

and binop = |Or |And |Xor |Addm |Mulm |Subm 

and binopi = |Ori | Andi |Xori |Addi |Lw |Sw

and monopi = |Li 

and monop = |Move |Smf |Divm

and prog = stmt list
