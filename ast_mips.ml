(* Syntaxe abstraite pour le langage MIPS *)

type stmt =
  | Sblockm of stmt list
  | Sbinopi of binopi*registre*registre*expr
  | Sbinop of binop*registre*registre*registre 
  | Smonopi of monopi*registre*expr
  | Smonop of monop*registre*registre
  | Sjump of jump
  | Ssyscall
  | Smf of registre

and expr =
  | Intm of int

and registre =
  |Zero |At |V0 |V1 |A of int |T of int |S of int |K0 |K1 |Gp |Sp |Fp |Ra |Hi |Lo |Pc

and jump =
  | J of expr
  | J_label of string
  | Jal of string

and binop = |Or |And |Xor |Addm |Divm |Mulm |Subm 

and binopi = |Ori | Andi |Xori |Addi |Lw |Sw

and monopi = |Li 

and monop = |Move



type label = { name : string ; body : stmt }
and prog = { labels : label list ; }