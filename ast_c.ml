(* Syntaxe abstraite pour le langage C *)

type stmt =
  | Sval of expr
  | Sblock of stmt list
  | Sprintint of expr
  | Sreturn of expr

and expr =
  | Const of const
  | Op of binop * expr * expr
  | Ecall of string * expr list

and const = 
  | Inti of int
  | Null

and binop = Add | Sub | Mul | Div | Mod

and types =
  |Int
  |Void

type def = { typ : types ; name : string ; args : string list ; body : stmt ; }
and prog = { defs : def list ; }