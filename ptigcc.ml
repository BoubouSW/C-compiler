open Printf
  
let _main = 
  (* Vérification de la ligne de commande *)
  if Array.length Sys.argv <> 2 then
    begin
      eprintf "usage: ./ptigcc.exe [file].c\n" ;
      exit 1
    end ;

   let ifile = Sys.argv.(1) in
  
  (* Vérification de l'extension .c *)
  if not (Filename.check_suffix ifile ".c") then begin
    eprintf "Le fichier d'entrée doit avoir l'extension .c\n";
    exit 1
  end;
  Reader.read_file ifile