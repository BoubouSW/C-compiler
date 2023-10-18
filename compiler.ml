open Ast_mips

let string_register = function
  | Zero->"$zero"
  | At->"$at"
  | A(i)-> "$a"^(string_of_int i )
  | V0 -> "$v0"
  | V1 -> "$v1"
  | Fp -> "$fp"
  | Gp -> "$gp"
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
  | Addm -> "add"
  | Subm -> "sub"
  | Mulm -> "mul"
  | Divm -> "div"
  | And -> "and"
  | Xor -> "xor"
  | Or-> "or"
let string_binopi = function
  | Addi -> "addi"
  | Andi -> "andi"
  | Xori -> "xori"
  | Ori-> "ori"
  | Lw-> "lw"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

  | SW-> "sw"

let string_monopi= function 
  |Li->"li"

let string_monop= function
  |Move->"move"

let rec string_stmt= function
|Sbinopi(op,r1,r2,Intm(i))->(match op with
  |Sw|Lw->"\t"^string_binopi op^"\t"^string_register r1^","string_of_int i^"("^string_registre r2^")\n"
  |_->"\t"^string_binopi op^"\t"^string_register r1^","^string_register r2^","^string_of_int i^"\n"
)
|Sbinop(op,r1,r2,r3)->"\t"^string_binop op^"\t"^string_register r1^","^string_register r2^","^string_register r3^"\n"
|Smonopi(op,r1,Int(i))->"\t"^string_monopi op^"\t"^string_register r1^","^string_of_int i^"\n"
|Smonop(op,r1,r2)->"\t"^string_monopi op^"\t"^string_register r1^","^string_register r2^"\n"
|Ssyscall->"\tsyscall\n"
|Sjump(j)->(match j with
  |J(lab)->"\tj\t"^lab^"\n"
  |Jal(lab)->"\tjal\t"^lab^"\n"
  |Jr(r1)->"\tjr\t"^string_register r1^"\n")
|Slabel(lab)->lab^":\n"


let print_program p out_filename =
  let out_file = open_out out_filename in
  let add s =
     Printf.fprintf out_file "%s" s;
  in
  add "\t.text";
  List.iter (fun e -> string_stmt e |> add )p ;
  add "\t.data";
  close_out out_file
