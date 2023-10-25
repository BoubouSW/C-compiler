{
  open Lexing
  open Parser
   
  exception Lexing_error of char
    
  let kwd_tbl = ["int",INT;"void",VOID;"printint",PRINTINT;"return",RETURN; "if", IF; "else", ELSE; "while", WHILE;]
  let id_or_kwd s = try List.assoc s kwd_tbl with _ -> IDENT s

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = letter (letter | digit)*
let integer = ['0'-'9']+
let space = [' ' '\t']
let comment = '/''/' [^'\n']*
let bcom = '/''*' ([^'\n']|'\n')* '*''/'

rule token = parse
  | '\n'    { newline lexbuf; token lexbuf }
  | space+  { token lexbuf }
  | ident as id { id_or_kwd id }
  | '+'     { PLUS }
  | '-'     { MINUS }
  | '*'     { TIMES }
  | '/'     { DIV }
  | '%'     { MOD }
  | '='     { EQ }
  | '('     { LP }
  | ')'     { RP }
  | ','     { COMMA }
  | ';'     { SEMICOLON }
  | '{'     { LB }
  | '}'     { RB }
  | '!'     { NOT }
  | '<''='  { LEQ }
  | '>''='  { GEQ }
  | '<'     { LE }
  | '>'     { GE }
  | '!''='  { NEQ }
  | '=''='  { EQQ }
  | '&''&'  { AND }
  | '|''|'  { OR }
  | '&'     { ESP }
  | integer as s { CST (int_of_string s) }
  | eof     { EOF }
  | '\r'    { newline lexbuf; token lexbuf }
  | comment as s { COM(s) }
  | bcom as s { BCOM(s) }
  | _ as c  { raise (Lexing_error c) }