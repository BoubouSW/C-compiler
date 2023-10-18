open Ast_c
open Ast_mips

let associe_binop op = match op with
  |Mul -> Mulm
  |Sub -> Subm
  |Add -> Addm
  | _ -> failwith "Pas un binop"

  let converti program = (*On stocke le resultat dans a*)
  
  let rec eval_binop op e1 e2 = match op,e1,e2 with
    |Add,Const(Inti(i)),e | Add,e,Const(Inti(i)) 
     -> (eval_expr e)@[Sbinopi(Addi,A(0),A(0),Intm(i))]
    
    |Sub,e,Const(Inti (i)) -> (eval_expr e)@[Sbinopi(Addi,A(0),A(0),Intm(-i))]

    |Mul,_,_|Sub,_,_|Add,_,_ -> (eval_expr e1)@[Sbinopi(Sw,A(0),Sp,Intm(0));
      Sbinopi(Addi,Sp,Sp,Intm(4))]@(eval_expr e2)@[Sbinopi(Addi,Sp,A(0),Intm(-4));
      Sbinopi(Lw,T(0),Sp,Intm(0));Sbinop(associe_binop op,A(0),A(0),T(0))]

    |Div,_,_ -> (eval_expr e1)@[Sbinopi(Sw,A(0),Sp,Intm(0));
    Sbinopi(Addi,Sp,Sp,Intm(4))]@(eval_expr e2)@[Sbinopi(Addi,Sp,A(0),Intm(-4));
    Sbinopi(Lw,T(0),Sp,Intm(0));Smonop(Divm,A(0),T(0));Smonop(Smf,A(0),Hi)]

    |Mod,_,_ -> (eval_expr e1)@[Sbinopi(Sw,A(0),Sp,Intm(0));
    Sbinopi(Addi,Sp,Sp,Intm(4))]@(eval_expr e2)@[Sbinopi(Addi,Sp,A(0),Intm(-4));
    Sbinopi(Lw,T(0),Sp,Intm(0));Smonop(Divm,A(0),T(0));Smonop(Smf,A(0),Lo)]

    and eval_expr e = match e with
    |Const(Inti(i)) -> [Smonopi(Li,A(0),Intm(i))]
    |Op(b,e1,e2) -> eval_binop b e1 e2;
    |Ecall(f,_) -> [Sjump(Jal(f))]
    |Const(Null) -> []

    and eval_stmt stmt = match stmt with
    |Sblock(l) -> List.fold_left (fun instr s-> instr@(eval_stmt s)) [] l
    |Sval(e) -> eval_expr e;
    |Sprintint(e) -> (eval_expr e) @ [Smonopi(Li,V0,Intm(1));Ssyscall]
    |Sreturn(e) -> (eval_expr e)@[Sjump(Jr(Ra))] in 

    List.fold_left (fun instr fonction -> instr@[Slabel(fonction.name)]@(eval_stmt fonction.body)) [] program.defs