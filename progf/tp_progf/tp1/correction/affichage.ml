(* ouverture des modules a utiliser			*)
open Graphics


(*  
   dessine_segment : (int*int) -> (int*int) -> unit
   Dessine un segment sur un écran préalablement créé.
   Parametre (xa,ya) : (int*int), coordonnées de la première extrémité du segment
   Paramètre (xb,yb) : (int*int), coordonnées de la seconde extrémité du segment
   Resultat : unit, affichage du segment sur l'écran
*)
let dessine_segment (xa,ya) (xb,yb) =
  moveto xa ya;
  lineto xb yb
