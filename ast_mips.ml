(* Syntaxe abstraite pour le langage MIPS *)

type stmt =
  | Sblock of stmt list
  | Sbinopi of binopi*registre*registre*expr
  | Sbinop of binop*registre*registre*registre 
  | Smonopi of monopi*registre*expr
  | Smonop of monop*registre*registre
  | Sjump of jump
  | Ssyscall

and expr =
  | Int of int

and registre =
  |Zero |At |V0 |V1 |A of int |T of int |S of int |K0 |K1 |Gp |Sp |Fp |Ra |Hi |Lo |Pc

and jump =
  | J of expr
  | J_label of string
  | Jal of string

and binop = |Or |And |Xor |Add |Div |Mul |Sub 

and binopi = |Ori | Andi |Xori |Addi |Subi |Lw |Sw

and monopi = |Li 

and monop = |Move



type def = { typ : const ; name : string ; args : string list ; body : stmt ; }
and prog = { defs : def list ; }