(* Arbre binaire de recherche d'entiers *)

(* Exercice 1 *)
type arbre = Noeud of int * ( (arbre option)*(arbre option) )

(* Exercice 2 *)
(*CONTRAT
Fonction qui insere une element e
Type : arbre->int->arbre
Paramètre : a  (arbre), arbre pour l'insertion
Paramètre : e (int), l'élement à ajouter
Résultat : l'arbre avec l'élement 
*)
let rec inserer noeud e = match noeud with
   | None ->  Noeud(e, (None, None))
   | Some Noeud(n, (a1, a2)) -> (* Serieusement la syntaxe de ce language est pourris. Voila 2h que je cherche pour 1 question*)
      if e=n then Noeud(n, (a1, a2))
      else if e<n then inserer a1 e
      else inserer a2 e

   (*CONTRAT
Fonction qui retire une element e si il est présent !
Remarque, on accelere la recherche en tirant profit du fait que l'arbre soit ordonné
(ie si la branche en question n'est pas dejà pleine)
Type : arbre->int->arbre
Paramètre : a  (arbre), arbre pour retirer
Paramètre : e (int), l'élement à retirer
Résultat : l'arbre avec l'élement 
*)
let rec retirer noeud e = match noeud with
   | None -> None
   | Some Noeud(n, (a1, a2)) ->
      if e=n then None
      else if e<n then Some (Noeud(n, (retirer a1 e, a2))) 
      else Some (Noeud(n, (a1, retirer a2 e)) )


(*CONTRAT
Fonction qui recherche une element e si il est présent !
Remarque, on accelere la recherche en tirant profit du fait que l'arbre soit ordonné
Remarque, on aurait donc pu utilisé rechercher dans retirer et inserer
Type : arbre->int->arbre
Paramètre : a  (arbre), arbre pour la recherche
Paramètre : e (int), l'élement à rechercher
Résultat : l'arbre avec l'élement 
*)
let rec rechercher noeud e = match noeud with
   | None -> None
   | Some Noeud(n, (a1, a2)) ->
      if e=n then noeud
      else if e<n then rechercher a1 e
      else rechercher a2 e

   
(* Exercice 3 *)
(*CONTRAT
Fonction qui parcours l'arbre en profondeur pour le transformer en liste ordonnée de ses éléments.
Remarque, on accelere la recherche en tirant profit du fait que l'arbre soit ordonné
Type : arbre->list int
Paramètre : a  (arbre), arbre pour la transformation
Résultat : la liste ordonnées des elements de l'arbre
*)
let rec parcourir noeud = match noeud with
   | None -> []
   | Some Noeud(n, (a1, a2)) -> [n]@(parcourir a1)@(parcourir a2)


(* Exercice 4 *)
(*CONTRAT
Fonction qui calcul la hauteur d'un arbre
Type : arbre->int
Paramètre : a  (arbre), arbre pour le calcul
Résultat : hauteur de l'arbre
*)
let rec hauteur noeud = match noeud with
   | None -> 1
   | Some Noeud(n, (a1, a2)) ->
      let h1 = hauteur a1 in
      let h2 = hauteur a2 in
      if h1<=h2 then h2
      else h1

(*CONTRAT
Fonction qui calcul la hauteur d'un arbre
Type : arbre->int
Paramètre : a  (arbre), arbre pour le calcul
Résultat : hauteur de l'arbre
*)
let rec cardinal noeud = match noeud with
   | None -> 1
   | Some Noeud(n, (a1, a2)) ->
      let h1 = hauteur a1 in
      let h2 = hauteur a2 in
      h1 + h2