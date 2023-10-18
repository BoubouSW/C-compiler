open ast_c
open ast_mips

let associe_binop op = match op with
  |Mul -> Mulm
  |Sub -> Subm
  |Div -> Divm
  | _ -> failwith "Pas un binop"

  let rec converti program = (*On stocke le resultat dans a*)
  
  let rec eval_binop op e1 e2 = match op,e1,e2 with
    |Add,Const(Inti(i)),e | Add,e,Const(Inti(i)) |Sub,e,Const(Inti(-i))
     -> (eval_expr e)@[Sbinopi(Addi,A0,A0,Intm(i))]

    |Mul|Sub|Add -> (eval_expr e1)@[Sbinopi(Sw,A0,Sp,Intm(0));
      Sbinopi(Addi,Sp,Sp,Intm(4))]@(eval_expr e2)@[Sbinopi(Addi,Sp,A0,Intm(-4));
      Sbinop(Lw,T(0),Sp);Sbinop(associe_binop op,A0,A0,T(0))]

    |Div -> (eval_expr e1)@[Sbinopi(Sw,A0,Sp,Intm(0));
    Sbinopi(Addi,Sp,Sp,Intm(4))]@(eval_expr e2)@[Sbinopi(Addi,Sp,A0,Intm(-4));
    Sbinop(Lw,T(0),Sp);Sbinop(Div,A0,T(0));Smonop(Smf,A0,Hi)]

    |Mod -> (eval_expr e1)@[Sbinopi(Sw,A0,Sp,Intm(0));
    Sbinopi(Addi,Sp,Sp,Intm(4))]@(eval_expr e2)@[Sbinopi(Addi,Sp,A0,Intm(-4));
    Sbinop(Lw,T(0),Sp);Sbinop(Div,A0,T(0));Smonop(Smf,A0,Lo)]

    and eval_expr e = match e with
    |Const(Inti(i)) -> [Smonopi(Li,A0,Intm(i))]
    |Op(b,e1,e2) -> eval_binop b e1 e2;

    and eval_stmt stmt = match stmt with
    |Sblock(l) -> List.fold_right (fun e instr -> fusionne (eval_expr stmt) instr) l []
    |Sval(e) -> eval_expr e;
    |Sprintint(expr) -> (eval_expr expr) @ [Smonopi(Li,V0,Intm(1));Ssyscall]
    |Sreturn(expr) -> (eval_expr e)@[Sjump(Jr(Ra))]

