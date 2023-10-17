open Lexing

open Compiler
open Lexer
open Parser

let read_file ifile =
  (*On ouvre le fichier*)
  let file = open_in ifile in

  
  let buffer = Lexing.from_channel file in

  (*On transforme le buffer en arbre de tokens*)
  (*let parsed = Parser.parse buffer*)

  (*On ferme le fichier*)
  close_in file;

  (*On compile le résultat du parser*)
  (*Compiler.compile parsed*)