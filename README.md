# C-compiler

Creating a C compiler to MIPS

Pour compiler :
`./ptigcc my_file.c`

Pour exécuter :
`spim -file my_file.c.s`

Le compilateur fonctionne de la manière suivante:
- 'parser' et 'lexer' convertissent le programme (d'extension .c) en liste d'instructions C ('ast_c')
- 'c_to_mips' convertit cette liste d'instructions C ('ast_c') en liste d'instructions Mips ('ast_mips')
- 'compiler' lit la liste d'instructions Mips ('ast_mips') et l'écrit dans un fichier d'extension .s
- 'reader' lit un fichier d'extension .c et appelle à la suite: lexer, parser, c_to_mips, compiler
- 'ptitgcc' prend un chemin vers un fichier d'extension .c et appelle 'reader' sur ce fichier
  
Nous avons fait toutes les fonctionnalités de base :
- un seul type (int)
- fonction à plusieurs arguments qui retourne une valeur
- if et if/else
- arithmetique, comparaisons et logique
- variable locale et assignation (on peut faire les deux en meme temps)
- fonction printint

Ensuite nous avons fait plusieurs extensions :
- boucle while et for (avec i++ possible)
- pointeurs (pas de distinction entre pointeur et integer)
- les commentaires (// et /**/)

Précisions:
- Le "else if()" n'est pas supporté, on utilisera "else { if () ... }"
- Les caractères spéciaux ne sont pas autorisés dans les identifiants de fonctions/variables
- On utilisera le modulo de la convention C pour les nombres négatifs (ex: '(-16) % 3' est égal à '-1' et non '-2')
