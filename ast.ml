(* Syntaxe abstraite pour le langage C *)

type stmt =
  | Sval of expr
  | Sblock of stmt list
  | Sprintint of int

and expr =
  | Const of const
  | Op of binop * expr * expr
  | Ecall of string * expr list

and const = 
  | Int of int
  | Void

and binop = Add

type def = { typ : const ; name : string ; args : string list ; body : stmt ; }
and prog = { defs : def list ; }