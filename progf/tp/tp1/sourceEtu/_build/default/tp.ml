(* open Graphics *)
(* open Affichage *)

(* Exercice 2 *)
(*  
   coeff_directeur : float*float -> float*float -> float
   calcule le coefficient directeur de la droite représentée par deux points
   Parametre (x1, y1) : float*float, le premier point
   Parametre (x2, y2) : float*float, le second point
   Resultat : float, le coefficient directeur de la droite passant par
   (x1, y1) et (x2, y2)
*)

let coeff_directeur (x1,y1) (x2,y2) = (y1-.y2)/.(x1-.x2)

let%test _ = coeff_directeur (0., 0.) (1., 2.) = 2.
let%test _ = coeff_directeur (1., 2.) (0., 0.) = 2.
let%test _ = coeff_directeur (0., 0.) (2., 1.) = 0.5
let%test _ = coeff_directeur (0., 0.) (-2., 1.) = -0.5
let%test _ = coeff_directeur (1., 2.) (2., 1.) = -1.


(* Exercice 3 *)
(* Les contrats et tests des fonctions ne sont pas demandés *)
(* f1 : int * int -> bool *)
(* f2 : int -> bool *)
(* f3 : 'a -> `a *)
(* f4 : `a * `a -> bool *)
(* f5 : `a*`b -> `a*)
let f1 (x,y) = (x=y)
let f2 (x) = x==0
let f3 x = x
let f4 (x,y) = x==y
let f5 (x,y) = x


(* Exercice 4 *)
(* ieme :  a*a*a->int->a *)
(* renvoie le ième élément d'un triplet *)
(* t : le triplet *)
(* i : l'indice de l'élèment dans le triplet *)
(* renvoie le ième élément t *)
(* précondition : 1 =< i =< 3 *)

let ieme t i = let (x,y,z) = t in match i with
   | 1 -> x
   | 2 -> y
   | 3 -> z
   | _ -> failwith("Entier non valide")
(* Son type est a*a*a->int->a *)

let%test _ = ieme (5,60,7) 1 = 5
let%test _ = ieme (5,60,17) 2 = 60
let%test _ = ieme (5,60,17) 3 = 17
let%test _ = ieme ('r','e','l') 1 = 'r'
let%test _ = ieme ('r','e','l') 2 = 'e'
let%test _ = ieme ('r','e','l') 3 = 'l'

(* Exercice 5 *)
(* PGCD -> pgcd.ml *)
(* renvoie le pgcd de a et de b *)
(* a : la 1er entier *)
(* b : le 2nd entier *)
(* renvoie le pgcd de a et b *)
(* précondition : a et b entier positif *)
let rec pgcd (a,b) =
   if a=b 
      then a 
   else 
      if a>b 
         then pgcd (a-b, b)
      else pgcd (a, b-a)

let pgcd_null (a,b) =
   if a=0 || b=0
      then 0
   else
      pgcd (a,b)

let pgcd_prected (a,b) =
   if a<0 || b<0
      then failwith("PGCD indéfinis pour a et b négatif")
   else
      pgcd_null (a,b)


(* Question perso : Qu'elle est la differences entre pgcd(a,b) et pgcd a b ?*)

(* Exercice 6 *)
(*  padovan : int -> int
Fonction qui calcule la nième valeur de la suite de Padovan : u(n+3) = u(n+1) + u(n); u(2)=1, u(1)=u(0)=0 
Paramètre n : un entier représentant la nième valeur à calculer
Précondition : n >=0
Résultat : un entier la nième valeur de la suite de Padovan 
*)

let rec padovan n =
   if n<0
      then failwith("Padovan indéfinis pour n négatif")
   else
      if n=0 || n=1
         then 0      (* u0=u1=0 *)
      else if n=2
         then 1      (* u2=1 *)
      else
         padovan(n-2) + padovan(n-3)

let%test _ = padovan 0 = 0 (* padovan(0) = 0 *)
let%test _ = padovan 1 = 0 (* padovan(1) = 1*)
let%test _ = padovan 2 = 1 (* padovan(2) = 1*)
let%test _ = padovan 3 = 0 (* padovan(3) = *)
let%test _ = padovan 4 = 1
let%test _ = padovan 5 = 1
let%test _ = padovan 6 = 1
let%test _ = padovan 7 = 2
let%test _ = padovan 8 = 2
let%test _ = padovan 9 = 3
let%test _ = padovan 10 = 4

(* Exercice 7 *)
(* estPremier : int -> bool*)
(* fonction qui indique si un nombre est premier *)
(* Paramètre n : un entier naturel dont on doit dire s'il est premier ou pas *)
(* Précondition : n >= 0 *)
(* Résultat : l'information de si n est premier ou pas *)

let estPremier n = failwith "TO DO"

let%test _ = estPremier 2
let%test _ = estPremier 3 
let%test _ = not (estPremier 4)
let%test _ = estPremier 5
let%test _ = not (estPremier 6)
let%test _ = estPremier 7
let%test _ = not (estPremier 8)
let%test _ = not (estPremier 9)
let%test _ = not (estPremier 10)
let%test _ = not (estPremier 0)
let%test _ = not (estPremier 1)

(*****************************)
(****** Bonus "ludique" ******)
(*****************************)


(*  Création de l'écran d'affichage *)
(* let _ = open_graph " 800x600" *)

(* Exercice 8 *)
(*  
   dragon : (int*int) -> (int*int) -> int -> unit
   Dessine la courbe du dragon à partir de deux points et d'une précision.
   Parametre (xa,ya) : (int*int), coordonnées de la première extrémité du dragon
   Paramètre (xb,yb) : (int*int), coordonnées de la seconde extrémité du dragon
   Paramètre n : nombre d'itération (plus n est grand, plus la courbe aura de détails)
   Resultat : unit, affichage de la courbe du dragon sur l'écran
   Précondition : n positif ou nul
*)

let dragon (xa,ya) (xb,yb) n = failwith "TO DO"

(* let%test_unit _ = dragon (200,350) (600,350) 20; *)

(*  Fermeture de l'écran d'affichage *)
(* close_graph() *)
