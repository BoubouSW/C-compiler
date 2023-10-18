open Format
open Lexing

(* localise une erreur en indiquant la ligne et la colonne *)
let localisation pos ifile =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" ifile l (c-1) c

let read_file ifile =
  (*On ouvre le fichier*)
  let file = open_in ifile in

  let buffer = Lexing.from_channel file in

  try
    (*On transforme le buffer en arbre de tokens*)
    let parsed = Parser.file Lexer.token buffer in

    (*On ferme le fichier*)
    close_in file;

    (*On compile le résultat du parser*)
    let mips = C_to_mips.converti parsed in
    Compiler.print_program mips (ifile ^ ".s")
  
  with
    | Lexer.Lexing_error c -> 
      (* Erreur lexicale. On r�cup�re sa position absolue et 
        on la convertit en num�ro de ligne *)
      localisation (Lexing.lexeme_start_p buffer) ifile;
      eprintf "Erreur dans l'analyse lexicale: %c@." c;
      exit 1
    | Parser.Error -> 
      (* Erreur syntaxique. On r�cup�re sa position absolue et on la 
        convertit en num�ro de ligne *)
      localisation (Lexing.lexeme_start_p buffer) ifile;
      eprintf "Erreur dans l'analyse syntaxique: %c@." c;
      exit 1
    | _ ->
      localisation (Lexing.lexeme_start_p buffer) ifile;
      eprintf "Une erreur inconnue est intervenue.\n";
      exit 1