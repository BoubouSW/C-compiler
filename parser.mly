%{
  open Ast_c
%}

/* def token */

%token <int> CST
%token <string> IDENT
%token INT VOID RETURN PRINTINT
%token EOF 
%token LP RP LB RB
%token PLUS MINUS TIMES DIV MOD
(*%token EQ*)
%token COMMA SEMICOLON

/* def priorites */

(*%right EQ*)
%left PLUS MINUS 
%left TIMES DIV MOD
(*%nonassoc uminus*)

%start file

%type <Ast_c.prog> file

%%

file: 
  | d = def* ; s = stmt+ ; EOF {{defs = {typ = Int; name="-#-main-#-";body=Sblock(s);args=[]}::d }} 
;

types:
  | INT { Int }
  | VOID { Void }

type_args_fun:
  | types i = IDENT { i }

def: tip = types ; nom = IDENT ; LP ; args = separated_list(COMMA,type_args_fun) ; RP ; bod =  suite 
  {{typ = tip ; name = nom ; args = args ; body = bod }}
;

suite:
  | LB ; s = list(terminated(stmt,SEMICOLON)) ; RB { Sblock(s) }
;
 
stmt:
  | PRINTINT e = expr { Sprintint e }
  | RETURN e = expr { Sreturn e }
;

const:
  | i = CST { Inti(i) }
;

expr:
  | c = const { Const c }
  | e1 = expr o = op e2 = expr { Op (o, e1, e2) }
  | LP e = expr RP { e }
;

%inline op:
  | PLUS  { Add }
  | MINUS { Sub }
  | TIMES { Mul }
  | DIV   { Div }
  | MOD   { Mod }
;