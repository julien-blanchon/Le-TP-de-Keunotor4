exception PenteInfinie
open Graphics
open Affichage

(*  
   coeff_directeur : float*float -> float*float -> float
   calcule le coefficient directeur de la droite représentée par deux points
   Parametre (x1, y1) : float*float, le premier point
   Parametre (x2, y2) : float*float, le second point
   Resultat : float, le coefficient directeur de la droite passant par
   (x1, y1) et (x2, y2)
*)
let coeff_directeur (x1,y1) (x2,y2) =
  (y2-.y1)/.(x2-.x1)

let%test _ = coeff_directeur (0., 0.) (1., 2.) = 2.
let%test _ = coeff_directeur (1., 2.) (0., 0.) = 2.
let%test _ = coeff_directeur (0., 0.) (2., 1.) = 0.5
let%test _ = coeff_directeur (0., 0.) (-2., 1.) = -0.5
let%test _ = coeff_directeur (1., 2.) (2., 1.) = -1.


(* Exercice 3 *)
(* int * int -> bool *)
let f1 = fun x y -> (x+y) > 0 
(* int -> bool *)
let f2 x = x < 0
(* 'a -> `a *)
let f3 x = x
(*`a * `a -> bool *)
let f4 = (=)
(*`a*`b -> `a*)
let f5 (x,_) = x


(*  pgcd : int -> int -> int
fonction qui calcule le pgcd de deux nombres entiers
Paramètres a et b : les deux entiers dont on calcule le pgcd
Résultat : le pgcd
*)
let pgcd a b = 
  let rec pgcd_positif a b = 
    if (a=b)
    then a
    else if (a > b)
         then pgcd_positif (a-b) (b)
         else pgcd_positif (a) (b-a)
  and valeur_absolue a = if (a >=0) then a else -a
in pgcd_positif (valeur_absolue a) (valeur_absolue b)

let%test _ = pgcd 4 6 = 2 
let%test _ = pgcd (-4) 6 = 2
let%test _ = pgcd (-4) (-6) = 2
let%test _ = pgcd 4 (-6) = 2
let%test _ = pgcd 3 7 = 1 
let%test _ = pgcd 10 10 = 10

(* Intérêt d'une fonction locale => calcule du pgcd plus simple sur les valeurs positives, donc décomposition fonctionnelle qui simplifie le problème *)



(*  padovan : int -> int
Fonction qui calcule la nième valeur de la suite de Padovan : u(n+3) = u(n+1) + u(n); u(2)=1, u(1)=u(0)=0 
Paramètre n : un entier représentant la nième valeur à calculer
Précondition : n >=0
Résultat : un entier la nième valeur de la suite de Padovan 
*)

let rec padovan n =
  match n with
  | 0 -> 0
  | 1 -> 0
  | 2 -> 1
  | _ -> (padovan (n-2)) + (padovan (n-3))


let%test _ = padovan 0 = 0
let%test _ = padovan 1 = 0 
let%test _ = padovan 2 = 1
let%test _ = padovan 3 = 0
let%test _ = padovan 4 = 1
let%test _ = padovan 5 = 1
let%test _ = padovan 6 = 1
let%test _ = padovan 7 = 2
let%test _ = padovan 8 = 2
let%test _ = padovan 9 = 3
let%test _ = padovan 10 = 4


(* Arbre des appels récursifs
-> padovan 9
   -> padovan 7
      -> padovan 5
         -> padovan 3
            -> padovan 1
            -> padovan 0
         -> padovan 2
      -> padovan 4
         -> padovan 2
         -> padovan 1
   -> padovan 6
      -> padovan 4
         -> padovan 2
         -> padovan 1
      -> padovan 3
         -> padovan 1
         -> padovan 0
*)

(*Calcul de la complexite
On prend en compte le nombre d'appels récursifs
C(0)=C(1)=C(2) = 0
C(n+3) = 2+ C(n+1) + C(n)
=> Calcule avec les fonctions génératrices
OU
A l'intuiton :
- Chaque appel génère : 2 appels récursifs
- On fait -2 au minimum à chaque fois , donc avant de retomber sur les cas terminaux : arbre de profondeur n/2
- Donc complexité en O(2^(n/2))
*)

(* ATTENTION : ils n'auront pas vu la récursivité terminale en cours, 
   mais c'est l'occasion qu'ils se posent la question avant que ça soit abordé au cours suivant *)

(*  padovan2 : int -> int
Fonction qui calcule la nième valeur de la suite de Padovan : u(n+3) = u(n+1) + u(n); u(2)=1, u(1)=u(0)=0 
Paramètre n : un entier représentant la nième valeur à calculer
Précondition : n >=0
Résultat : un entier la nième valeur de la suite de Padovan 
*)
let padovan2 n =
  (* les paramètres de la fonction auxiliaire sont :
    - k : l'itération
    - pk : u(k)
    - pkm1 : u(k-1)
    - pkm2 : u(k-2)
  *)
  let rec padovan_aux k pk pkm1 pkm2 =
    if (k=n) then pk
    else (padovan_aux (k+1) (pkm1+pkm2) pk pkm1 )
  in
      match n with
  | 0 -> 0
  | 1 -> 0
  | 2 -> 1
  | _ -> padovan_aux 2 1 0 0 

let%test _ = padovan2 0 = 0
let%test _ = padovan2 1 = 0 
let%test _ = padovan2 2 = 1
let%test _ = padovan2 3 = 0
let%test _ = padovan2 4 = 1
let%test _ = padovan2 5 = 1
let%test _ = padovan2 6 = 1
let%test _ = padovan2 7 = 2
let%test _ = padovan2 8 = 2
let%test _ = padovan2 9 = 3
let%test _ = padovan2 10 = 4

(* Calcul de la complexité : 
C(0)=C(1)=C(2) = 0
C(n+1) = 1+C(n)
=> Complexité linéaire
*)

(* estPremier : int -> bool
fonction qui indique si un nombre est premier
Paramètre n : un entier naturel dont on doit dire s'il est premier ou pas
Précondition : n >= 0
Résultat : l'information de si n est premier ou pas
*)

let estPremier n = 
  let rec estPremier_aux n m =
    (n=m)|| ( (not (n mod m = 0)) && (estPremier_aux n (m+1)))
  in n >= 2 && estPremier_aux n 2


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



(*  Création de l'écran d'affichage *)
let _ = open_graph " 800x600"

(*  
   dragon : (int*int) -> (int*int) -> int -> unit
   Dessine la courbe du dragon à partir de deux points et d'une précision.
   Parametre (xa,ya) : (int*int), coordonnées de la première extrémité du dragon
   Paramètre (xb,yb) : (int*int), coordonnées de la seconde extrémité du dragon
   Paramètre n : nombre d'itération (plus n est grand, plus la courbe aura de détails)
   Resultat : unit, affichage de la courbe du dragon sur l'écran
   Précondition : n positif ou nul
*)
let rec dragon (xa,ya) (xb,yb) n = 
  if n = 0
  then dessine_segment (xa,ya) (xb,yb)
  else 
    let xc = (xa+xb)/2 + (yb-ya)/ 2 in
    let yc = (ya+yb)/2 + (xa-xb)/2 in
    dragon (xa,ya) (xc,yc) (n-1);
    dragon (xb,yb) (xc,yc) (n-1)

let%test_unit _ = dragon (200,350) (600,350) 20;


(*  Fermeture de l'écran d'affichage *)
 close_graph()
