open ast_c



let rec converti program =
  let rec eval_expr e = match e with
    |Const(Inti(n)) -> ast_mips.Int()  
    |Op(ope,e1,e2)
    | _ -> _



  and eval_stmt s = match s with
    |Sval(e) -> eval_expr e
    |

