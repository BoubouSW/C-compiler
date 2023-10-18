let string_register = function
  | Zero->"$zero"
  | At->"$at"
  | A(i)-> "$a"^(string_of_int i )
  | V0 -> "$v0"
  | V1 -> "$v1"
  | RA -> "$ra"
  | SP -> "$sp"
  | FP -> "$fp"
  | GP -> "$gp"
  | T i -> "$t"^(string_of_int i )
  | S i -> "$t"^(string_of_int i )
  | K0  ->"$k0"
  | K1 -> "$k1"
  | Gp ->"$gp"
  | Sp -> "$sp"
  | Fp ->"$fp"
  | Ra -> "$ra"
  | Hi -> "$hi"
  | Lo -> "$lo"
  | Pc -> "$sp"
         
let string_binop = function
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"
  | And -> "and"
  | Xor -> "xor"
  | Or-> "or"
let string_binopi = function
  | Addi -> "addi"
  | Andi -> "andi"
  | Xori -> "xori"
  | Ori-> "ori"
  | Lw-> "lw"
  | SW-> "sw"

let string_monopi= function 
  |Li->"li"

let string_monop= function
  |Move->"move"

let rec string_stmt= function
|Sblock(l)->List.fold_left (fun x y-> x^y) (List.map string_stmt l)
|Sbinopi(op,r1,r2,i)->"\t"^string_binopi op^"\t"^string_register r1^","^string_register r2^","^int_of_string i^"\n"
|Sbinop(op,r1,r2,r3)->"\t"^string_binopi op^"\t"^string_register r1^","^string_register r2^","^int_of_string i^"\n"




let string_instruction = function
  | Move (dst, src) -> 
      "\tmove\t"^(string_register dst)^", "^(string_register src)
  | Li (r, i) ->
     "\tli\t"^(string_register r)^", "^(string_of_int i)
  | Lw (r, a) ->
    "\tlw\t"^(string_register r)^","^(string_address a)
  | Sw (r, a) ->
     "\tsw\t"^(string_register r)^","^(string_address a)
  | Arith (op, dst, src, src2) ->
     "\t"^(string_arith op)^"\t"^(string_register dst)^","^(string_register src)^","^(string_register src2)
  | Arithi (op, dst, src, src2) ->
     "\t"^(string_arith op)^"\t"^(string_register dst)^","^(string_register src)^","^(string_of_int src2)
  | Jal s -> "\tjal\t"^s
  | J s -> "\tj\t"^s
  | Jr r -> "\tjr\t"^(string_register r)
  | Syscall -> "\tsyscall"
  | Comment s ->  "\t "^s
  | Label s ->  s^":"

let string_data = function
  | Asciiz (l, s) -> 
      l^":\t.asciiz '"^(String.escaped s)^"'"
  | Word (l, n) ->
     l^": \t.word "^(string_of_int n)
let print_program p out_filename =
  let out_file = open_out out_filename in
  let add s =
     Printf.fprintf out_file "%s\n" s;
  in
  add "\t.text";
  List.iter (fun e -> string_instruction e |> add ) p.text  ;
  add "\t.data";
  List.iter (fun e -> string_data e |> add ) p.data ;
  close_out out_file
