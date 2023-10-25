(* Syntaxe abstraite pour le langage C *)

type stmt =
  | Sval of expr
  | Sblock of stmt list
  | Sprintint of expr
  | Sreturn of expr
  | Svar of types*string
  | Sassign of string*expr
  | Sif of expr*stmt*stmt
  | Swhile of expr*stmt

and args_fun =
  | Args of types*string

and expr =
  | Const of const
  | Var of string
  | Minus of expr
  | Not of expr
  | Op of binop * expr * expr
  | Ecall of string * expr list

and const = 
  | Inti of int
  | Null

and binop = Add | Sub | Mul | Div | Mod | Leq | Le | Geq | Ge | Neq | Eq | And | Or

and types =
  |Int
  |Void

type def = { typ : types ; name : string ; args : args_fun list ; body : stmt ; }
and prog = { defs : def list ; }