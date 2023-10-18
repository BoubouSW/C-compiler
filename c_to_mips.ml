open ast_c

let rec fusionne b1 b2 = match b1,b2 with
  |Ast_mips.Sblock(l1),Ast_mips.Sblock(l2) -> Ast_mips.Sblock(l1@l2)

let associe_binop b = match binop with
  |Mul -> Ast_mips.Mul
  |Sub -> Ast_mips.Sub
  |Div -> Ast_mips.Div
  | _ -> failwith "Pas un binop"

  let rec converti program = (*On stocke le resultat dans v0*)
  
  let rec eval_binop op e1 e2 = match op,e1,e2 with
    |Add,Const(Inti(i)),_ | Add,_,Const(Inti(i)) |Sub,_,Const(Inti(-i))
     -> [Ast_mips.Sbinopi(Ast_mips.Addi,Ast_mips.V0,Ast_mips.V0,Int(i))]
    |Mul|Sub|Add -> (eval_expr e1)@[Ast_mips.Sbinopi(Ast_mips.Sw,Ast_mips.Sp,Ast_mips.V0,Ast_mips.Int(0));
      Sbinopi(Ast_mips.Addi,Ast_mips.)Ast_mips.Sbinop(associe_binop b)]
    |

  and eval_expr e = match e with
    |


     and eval_stmt stmt = match stmt with
    |Sblock(l) -> List.fold_right (fun e instr -> fusionne (eval_expr stmt) instr) l []
    |S

