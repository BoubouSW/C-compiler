%{
  open Ast_c
%}

/* def token */

%token <int> CST
%token <string> IDENT
%token INT VOID RETURN PRINTINT
%token AND OR NOT
%token LEQ GEQ LE GE EQQ NEQ
%token EOF 
%token LP RP LB RB
%token PLUS MINUS TIMES DIV MOD
(*%token EQ*)
%token COMMA SEMICOLON

/* def priorites */

(*%right EQ*)
%left OR
%left AND
%left EQQ NEQ
%left LEQ GEQ LE GE
%left PLUS MINUS 
%left TIMES DIV MOD
%nonassoc uminus not

%start file

%type <Ast_c.prog> file

%%

file: 
  | d = def* ; EOF {{defs = d }} 
;

types:
  | INT { Int }
  | VOID { Void }

type_args_fun:
  | typ = types ; i = IDENT { Args(typ,i) }

def: tip = types ; nom = IDENT ; LP ; args = separated_list(COMMA,type_args_fun) ; RP ; bod =  suite 
  {{typ = tip ; name = nom ; args = args ; body = bod }}
;

suite:
  | LB ; s = list(stmt) ; RB { Sblock(s) }
;
 
stmt:
  | PRINTINT LP e = expr RP SEMICOLON { Sprintint e }
  | RETURN e = expr SEMICOLON { Sreturn e }
  | typ = types nom = IDENT SEMICOLON { Svar(typ,nom) }
;

const:
  | i = CST { Inti(i) }
;

expr:
  | c = const { Const c }
  | NOT e = expr %prec not { Not(e) }
  | MINUS e = expr %prec uminus { Minus(e) } 
  | e1 = expr o = op e2 = expr { Op (o, e1, e2) }
  | LP e = expr RP { e }
;

%inline op:
  | PLUS  { Add }
  | MINUS { Sub }
  | TIMES { Mul }
  | DIV   { Div }
  | MOD   { Mod }
  | LEQ   { Leq }
  | GEQ   { Geq }
  | GE    { Ge  }
  | LE    { Le  }
  | NEQ   { Neq }
  | EQQ   { Eq  }
  | AND   { And }
  | OR    { Or  }
;