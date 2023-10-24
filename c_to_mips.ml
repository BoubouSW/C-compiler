open Ast_c
open Ast_mips

let variables  = Hashtbl.create 100

let associe_binop op = match op with
  |Mul -> Mulm
  |Sub -> Subm
  |Add -> Addm
  | _ -> print_string "Pas codee associe_binop";failwith "Pas un binop"

let converti program = (*On stocke le resultat dans a*)
  
  let rec eval_binop op e1 e2 off_set = match op,e1,e2 with
    |Add,Const(Inti(i)),e | Add,e,Const(Inti(i)) 
     -> (eval_expr e off_set)@[Sbinopi(Addi,A(0),A(0),Intm(i))]
    
    |Sub,e,Const(Inti (i)) -> (eval_expr e off_set)@[Sbinopi(Addi,A(0),A(0),Intm(-i))]

    |Mul,_,_|Sub,_,_|Add,_,_ -> (eval_expr e1 off_set)@[Sbinopi(Sw,A(0),Sp,Intm(-4*off_set))]
      @(eval_expr e2 (off_set+1))@[Sbinopi(Lw,T(0),Sp,Intm(-4*off_set));Sbinop(associe_binop op,A(0),A(0),T(0))]

    |Div,_,_ -> (eval_expr e1 off_set)@[Sbinopi(Sw,A(0),Sp,Intm(-4*off_set))]@
    (eval_expr e2 (off_set+1))@[Sbinopi(Lw,T(0),Sp,Intm(-4*off_set));Smonop(Divm,A(0),T(0));Smonop(Smf,A(0),Hi)]

    |Mod,_,_ -> (eval_expr e1 off_set)@[Sbinopi(Sw,A(0),Sp,Intm(-4*off_set))]@
    (eval_expr e2 (off_set+1))@[Sbinopi(Lw,T(0),Sp,Intm(-4*off_set));Smonop(Divm,A(0),T(0));Smonop(Smf,A(0),Lo)]

    |_ -> print_string "Pas codee eval_binop"; failwith "Pascodee "

    and eval_expr e off_set = match e with
    |Minus(expr) -> eval_binop Sub (Const(Inti 0)) expr off_set
    |Const(Inti(i)) -> [Smonopi(Li,A(0),Intm(i))]
    |Op(b,e1,e2) -> eval_binop b e1 e2 off_set
    |Ecall(f,l) -> 
      let assigne_les_variables = List.concat (List.mapi 
        (fun i arg ->
          (eval_expr arg (off_set+i+1))@[Sbinopi(Sw,A(0),Sp,Intm(-4*(off_set+i+1)))]) l) in
      [Sbinopi(Addi,Sp,Sp,Intm(-4*off_set))]@assigne_les_variables@[Sjump(Jal(f));Sbinopi(Addi,Sp,Sp,Intm(4*off_set))]
    |Const(Null) -> []
    |Var(s) -> 
      (try 
        [Sbinopi(Lw,A(0),Sp,Hashtbl.find variables s)] 
      with Not_found -> print_string ("variable "^s^" non definie\n");
    failwith "undefined")
    |_ -> print_string "Pas codee eval_expr";failwith "Pascodee "

    and eval_stmt ?(main=false) stmt off_set = match stmt with
    |Sblock(l) -> List.fold_left (fun instr s-> instr@(eval_stmt ~main:false s off_set)) [] l
    |Sval(e) -> eval_expr e off_set
    |Sprintint(e) -> (eval_expr e off_set) @ [Smonopi(Li,V0,Intm(1));Ssyscall]
    |Sreturn(e) when main = true -> (eval_expr e off_set)@[Smonopi(Li,V0,Intm(10));Ssyscall]
    |Sreturn(e) -> (eval_expr e off_set)@[Sbinopi(Lw,Ra,Sp,Intm(0));Sjump(Jr(Ra))]
    |_ -> print_string "Pas codee eval_stmt"; failwith "Pascodee "

  in 
    List.fold_left 
    (fun instr fonction ->
      List.iteri (fun i (Args(_,arg)) -> Hashtbl.add variables arg (Intm(i+1))) fonction.args;
      let evalue_la_fonction =eval_stmt ~main:(fonction.name="main") fonction.body (1+(List.length fonction.args)) in
      List.iter (fun (Args(_,arg)) -> Hashtbl.remove variables arg) fonction.args;
      instr@[Slabel(fonction.name);Sbinopi(Sw,Ra,Sp,Intm(0))]
      @evalue_la_fonction)
    [] program.defs