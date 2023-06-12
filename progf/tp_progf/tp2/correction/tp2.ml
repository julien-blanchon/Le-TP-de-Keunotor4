(******* TRIS ******)

(*  Tri par insertion **)

(*CONTRAT
Fonction qui ajoute un élément dans une liste triée, selon un odre donné
Type : ('a->'a->bool)->'a->'a list -> 'a list
Paramètre : ordre  ('a->'a->bool), un ordre sur les éléments de la liste
Paramètre : elt, l'élement à ajouter
Paramètre : l, la liste triée dans laquelle ajouter elt
Résultat : une liste triée avec les éléments de l, plus elt
*)
let rec insert ordre elt l =
  match l with
  |[]->[elt]
  |t::q -> if (ordre t elt)
           then t::(insert ordre elt q)
           else elt::l

(* TESTS *)
let%test _ = insert (fun x y -> x<y) 3 []=[3]
let%test _ = insert (fun x y -> x<y) 3 [2;4;5]=[2;3;4;5]
let%test _ = insert (fun x y -> x > y) 6 [3;2;1]=[6;3;2;1]



(*CONTRAT
Fonction qui trie une liste, selon un ordre donné
Type : ('a->'a->bool)->'a list -> 'a list
Paramètre : ordre  ('a->'a->bool), un ordre sur les éléments de la liste
Paramètre : l, la liste à trier
Résultat : une liste triée avec les éléments de l
*)
(*
let rec tri_insertion ordre l =
  match l with
  |[]->[]
  |t::q -> insert ordre t (tri_insertion ordre q)
  *)


let tri_insertion ordre l = List.fold_right (insert ordre) l []

(* TESTS *)
let%test _ = tri_insertion (fun x y -> x<y) [] =[]
let%test _ = tri_insertion (fun x y -> x<y) [4;2;4;3;1] =[1;2;3;4;4]
let%test _ = tri_insertion (fun x y -> x > y) [4;7;2;4;1;2;2;7]=[7;7;4;4;2;2;2;1]


(*  Tri fusion **)

(* CONTRAT
Fonction qui décompose une liste en deux listes de tailles égales à plus ou moins un élément
Paramètre : l, la liste à couper en deux
Retour : deux listes
*)
let rec scinde l = 
  match l with
  |[] -> [],[]
  |[a]->[a],[]
  |a::b::q -> let (l1,l2) = scinde q in
              (a::l1,b::l2)

(* TESTS *)
let%test _ = scinde [1;2;3;4] = ([1;3],[2;4])
let%test _ = scinde [1;2;3] = ([1;3],[2])
let%test _ = scinde [1] = ([1],[])
let%test _ = scinde [] = ([],[])


(* Fusionne deux listes triées pour en faire une seule triée
Paramètre : ordre  ('a->'a->bool), un ordre sur les éléments de la liste
Paramètre : l1 et l2, les deux listes triées
Résultat : une liste triée avec les éléments de l1 et l2
*)
let rec fusionne ordre l1 l2 =
  match l1, l2 with
  |[],_ -> l2
  |_,[] -> l1
  |a::q1,b::q2 ->
	if (ordre a b )
 	then a::(fusionne ordre q1 l2)
        else b::(fusionne ordre l1 q2)

(*TESTS*)
let%test _ = fusionne (fun x y -> x<y) [1;2;4;5;6] [3;4] = [1;2;3;4;4;5;6]
let%test _ = fusionne (fun x y -> x<y) [1;2;4] [3;4] = [1;2;3;4;4]
let%test _ = fusionne (fun x y -> x<y) [1;2;4] [3;4;8;9;10] = [1;2;3;4;4;8;9;10]
let%test _ = fusionne (fun x y -> x<y) [] [] = []
let%test _ = fusionne (fun x y -> x<y) [1] [] = [1]
let%test _ = fusionne (fun x y -> x<y) [] [1] = [1]
let%test _ = fusionne (fun x y -> x<y) [1] [2] = [1;2]
let%test _ = fusionne (fun x y -> x>y) [1] [2] = [2;1]


(* CONTRAT
Fonction qui trie une liste, selon un ordre donné
Type : ('a->'a->bool)->'a list -> 'a list
Paramètre : ordre  ('a->'a->bool), un ordre sur les éléments de la liste
Paramètre : l, la liste à trier
Résultat : une liste triée avec les éléments de l
*)
let rec tri_fusion ordre l =
  match l with
  |[] -> []
  |[a]-> [a] (* la taille du problème doit STRICTEMENT décroître *)
  | _ -> let (l1,l2) = scinde l in
         fusionne ordre (tri_fusion ordre l1) (tri_fusion ordre l2)


(* TESTS *)
let%test _ = tri_fusion (fun x y -> x<y) [] =[]
let%test _ = tri_fusion (fun x y -> x<y) [4;2;4;3;1] =[1;2;3;4;4]
let%test _ = tri_fusion (fun x y -> x > y) [4;7;2;4;1;2;2;7]=[7;7;4;4;2;2;2;1]


(* Pourquoi découper en deux listes de même taille -> sinon on peut se retrouver avec du n2 (1 elt d'un côté et tous les autres de l'autre) *)
(* tri rapide, pas rapide du tout pour une liste déjà triée *)



(*  Parsing du fichier *)

(* Affiche un quadruplet composé 
- du sexe des personnes ayant reçu ce prénom : 1 pour les hommes, 2 pour les femmes
- du prénom
- de l'année
- du nombre de fois où ce prénom a été donné cette année là
*)
let print_stat (sexe,nom,annee,nb) =
  Printf.eprintf "%s,%s,%d,%d%!\n" (if (sexe=1) then "M" else "F") nom annee nb

(* Analyse le fichier nat2016.txt (stratistique des prénoms entre 1900 et 2016) 
 et construit une liste de quadruplet (sexe,prénom,année,nombre d'affectation)
*)
let listStat = 
   let input = open_in "../../nat2016.txt" in  (* dune runtest *)
  (* let input = open_in "nat2016.txt" in *) (* dune utop *)
  (*let input = open_in "/mnt/n7fs/ens/tp_guivarch/pf/nat2016.txt" in *) (* étudiants *)
  let filebuf = Lexing.from_channel input in
  Parser.main Lexer.token filebuf
  

(* Analyse le fichier nathomme2016.txt (stratistique des prénoms d'homme commençant par un A ou un B entre 1900 et 2016) 
 et construit une liste de quadruplet (sexe,prénom,année,nombre d'affectation)
*)
let listStatHomme = 
  let input = open_in "../../nathomme2016.txt" in  (* dune runtest *)
 (* let input = open_in "nathomme2016.txt"  in *)  (* dune utop *)
  (* let input = open_in "/mnt/n7fs/ens/tp_guivarch/pf/nathomme2016.txt" in *) (* étudiants *)
  let filebuf = Lexing.from_channel input in
  Parser.main Lexer.token filebuf
  


(*  Tests des algorithmes de tri *)
let debutinsertion = Sys.time ()
let%test_unit _ = let _ = tri_insertion (fun (_s1,_p1,_a1,_nb1) (_s2,_p2,_a2,_nb2) -> _nb1 > _nb2) listStatHomme in () (* Le test échoue *)
let fininsertion = Sys.time ()
let%test_unit _ = let _ = tri_fusion (fun (_s1,_p1,_a1,_nb1) (_s2,_p2,_a2,_nb2) -> _nb1 > _nb2) listStatHomme in () (* Le test échoue *)
let finfusion = Sys.time ()
let%test _ = ((finfusion-.fininsertion) < fininsertion-.debutinsertion) (* 9000 fois plus d'opérations mais un facteur 350/400 sur le temps? ???*)

let%test_unit _ = let _ = tri_fusion (fun (_s1,_p1,_a1,_nb1) (_s2,_p2,_a2,_nb2) -> _nb1 > _nb2) listStat in ()  (* Le test échoue *)
let%test_unit _ = let _ = List.sort (fun (_s1,_p1,_a1,_nb1) (_s2,_p2,_a2,_nb2) -> if (_nb1 > _nb2) then 1 else if (_nb1=_nb2) then 0 else -1) listStat in ()  (* le test passe *)


(*** Tri plus efficace en rendant fusionne récursif terminal ***)


(* CONTACT
Fonction qui décompose une liste en deux listes de tailles égales à plus ou moins un élément
Paramètre : l, la liste à couper en deux
Retour : deux listesa liste á trier
*)
let scinde_rec_t l = 
   let rec aux_rec_t l n accu =
    if n=0
    then (l,accu)
    else match l with
         | [] -> ([],accu)
         |t::q -> aux_rec_t q (n-1) (t::accu)
  in aux_rec_t l ((List.length l)/2) [] 

(* TESTS *)
  let%test _ = scinde_rec_t [1;2;3;4;5;6;7;8;9] = ([5;6;7;8;9],[4;3;2;1])
  let%test _ = scinde_rec_t [1;2;3;4] = ([3;4],[2;1])
  let%test _ = scinde_rec_t [1;2;3] = ([2;3],[1])
  let%test _ = scinde_rec_t [1] = ([1],[])
  let%test _ = scinde_rec_t [] = ([],[])

(* Fusionne deux listes triées pour en faire une seule triée dans l'ordre inverse
Paramètre : ordre  ('a->'a->bool), un ordre sur les élements de la liste
Paramètre : l1 et l2, les deux listes triées selon l'ordre
Résultat : une liste triée avec les elements de l1 et l2
*)
let fusionne_rec_t ordre l1 l2 =
  let rec aux_rec_t l1 l2 accu =
    match l1, l2 with
    |[],_ -> List.rev_append l2 accu
    |_,[] -> List.rev_append l1 accu
    |a::q1,b::q2 ->
	    if (ordre a b )
 	    then aux_rec_t q1 l2 (a::accu)
      else aux_rec_t l1 q2 (b::accu)
  in aux_rec_t l1 l2 []

(*TESTS*)
let%test _ = fusionne_rec_t (fun x y -> x<y) [1;2;4;5;6] [3;4] = (List.rev [1;2;3;4;4;5;6])
let%test _ = fusionne_rec_t (fun x y -> x<y) [1;2;4] [3;4] = (List.rev [1;2;3;4;4])
let%test _ = fusionne_rec_t (fun x y -> x<y) [1;2;4] [3;4;8;9;10] = (List.rev [1;2;3;4;4;8;9;10])
let%test _ = fusionne_rec_t (fun x y -> x<y) [] [] = (List.rev [])
let%test _ = fusionne_rec_t (fun x y -> x<y) [1] [] = (List.rev [1])
let%test _ = fusionne_rec_t (fun x y -> x<y) [] [1] = (List.rev [1])
let%test _ = fusionne_rec_t (fun x y -> x<y) [1] [2] = (List.rev [1;2])
let%test _ = fusionne_rec_t (fun x y -> x>y) [1] [2] = (List.rev [2;1])

(* CONTRAT
Fonction qui trie une liste, selon un ordre donné
Type : ('a->'a->bool)->'a list -> 'a list
Paramètre : ordre  ('a->'a->bool), un ordre sur les élements de la liste
Paramètre : l, la liste à trier
Résultat : une liste triée avec les éléments de l
*)
let tri_fusion_rec_t ordre l =
  (* tri selon la négation de l'ordre *)
  let rec aux_neg l =
    match l with
    |[] -> []
    |[a]-> [a]
    | _ -> let (l1,l2) = scinde_rec_t l in
          (* Les sous-listes sont triée selon l'ordre *)
          (* Pour valider la pré-condition de fusion, il faut bien ordre en paramètre de fusionne *)
          (* La post condition de fusion nous donne bien la liste triée selon la négation de l'ordre *)
           fusionne_rec_t ordre (aux_pos l1) (aux_pos l2) 
  and aux_pos l =
    match l with
    |[] -> []
    |[a]-> [a]
    | _ -> let (l1,l2) = scinde_rec_t l in
          (* Les sous-listes sont triée selon la négation de l'ordre *)
          (* Pour valider la pré-condition de fusion, il faut bien la négation de l'ordre en paramètre de fusionne *)
          (* La post condition de fusion nous donne bien la liste triée selon la négation de la négation de l'ordre *)
          (* et donc triée selon l'ordre *)
           fusionne_rec_t (fun x y -> not (ordre x y)) (aux_neg l1) (aux_neg l2) 
  in aux_pos l


(* TESTS *)
let%test _ = tri_fusion_rec_t (fun x y -> x<y) [] =[]
let%test _ = tri_fusion_rec_t (fun x y -> x<y) [4;2;4;3;1] =[1;2;3;4;4]
let%test _ = tri_fusion_rec_t (fun x y -> x > y) [4;7;2;4;1;2;2;7]=[7;7;4;4;2;2;2;1]

let%test_unit _ = let _ = tri_fusion_rec_t (fun (_s1,_p1,_a1,_nb1) (_s2,_p2,_a2,_nb2) -> _nb1 > _nb2) listStat in ()  (* Le test passe!!! *)

(* Garde les n premiers éléments d'une liste *)
(* type : int -> 'a list -> 'a list *)
(* paramètre n : le nombre d'éléments à garder *)
(* paramètre l : la liste à couper *)
(* Si la liste a moins de n éléments, elle renvoyée *)
let rec tronque n l =
  if (n = 0 || l=[])
  then []
  else (List.hd l)::(tronque (n-1) (List.tl l) )


(* TESTS *)
let%test _ = tronque 0 [1;2;3;4;5;6] = []
let%test _ = tronque 1 [1;2;3;4;5;6] = [1]
let%test _ = tronque 2 [1;2;3;4;5;6] = [1;2]
let%test _ = tronque 3 [1;2;3;4;5;6] = [1;2;3]
let%test _ = tronque 4 [1;2;3;4;5;6] = [1;2;3;4]
let%test _ = tronque 5 [1;2;3;4;5;6] = [1;2;3;4;5]
let%test _ = tronque 6 [1;2;3;4;5;6] = [1;2;3;4;5;6]

(* Renvoie les n premiers éléments d'une liste poru un ordre donnée*)
(* type : ( 'a-> 'a -> bool) -> int -> 'a list -> 'a list *)
(* paramètre ordre : l'ordre utilisé pour comparer les éléments *)
(* paramètre l : la liste dans laquelle on cherche les éléments *)
(* paramètre n : le nombre d'éléments à trouver *)
(* Erreur si n est plus grand que la taille de la liste *)
let npremier_bete ordre n l = tronque n (tri_fusion_rec_t ordre l) 

(* TESTS *)
let%test _ =  npremier_bete (fun x y -> x > y) 3 [4;7;2;4;1;2;2;7] = [7;7;4]
let%test _ =  npremier_bete (fun x y -> x < y) 3 [4;7;2;4;1;2;2;7] = [1;2;2]


(* Supprime le dernier élément d'une liste 
Erreur si liste vide
*)
let rec enleveDernier l = 
  match l with
  |[] -> failwith "Liste vide!"
  |[_] -> []
  |t::q -> t::(enleveDernier q)

(* TESTS *)
let%test _ =  enleveDernier [4;7;2;4;1;2;2;7] = [4;7;2;4;1;2;2]


(*CONTRAT
Fonction qui ajoute un élément dans une liste triée, 
L'élément sera inséré à sa place s'il est plus petit que le dernier élément
L'élément sera pas inséré sinon
Type : ('a->'a->bool)->'a->'a list ->  'a list
Paramètre : ordre  ('a->'a->bool), un ordre sur les éléments de la liste
Paramètre : elt, l'élement à ajouter
Paramètre : l, la liste triée dans laquelle ajouter elt
Résultat : une liste triée de même taille que la liste initiale
*)
let rec insert_bornee ordre elt l =
  match l with
  |[]->[]
  |[t] -> if (ordre t elt) then [t] else [elt]
  |t::q -> if (ordre t elt)
           then t::(insert_bornee ordre elt q)
           else elt::(enleveDernier l)


(* TESTS *)
let%test _ = insert_bornee (fun x y -> x<y) 3 [] =[]
let%test _ = insert_bornee (fun x y -> x<y) 3 [2;4;5] =[2;3;4]
let%test _ = insert_bornee (fun x y -> x > y) 6 [3;2;1] =[6;3;2]
let%test _ = insert_bornee (fun x y -> x > y) 4 [3;2;1] =[4;3;2]
let%test _ = insert_bornee (fun x y -> x > y) 0 [3;2;1] =[3;2;1]
let%test _ = insert_bornee (fun x y -> x > y) 4 [7;6;5;3;2;1] =[7;6;5;4;3;2]
let%test _ = insert_bornee (fun x y -> x > y) 0 [7;6;5;3;2;1] =[7;6;5;3;2;1]


(* Renvoie les n premiers éléments d'une liste poru un ordre donnée*)
(* type : ( 'a-> 'a -> bool) -> int -> 'a list -> 'a list *)
(* paramètre ordre : l'ordre utiliser pour comparer les éléments *)
(* paramètre l : la liste dans laquelle on cherche les éléments *)
(* paramètre n : le nombre d'éléments à trouver *)
let npremier ordre n l =
  let rec aux_bornee l accu =
    match l with
    |[]->accu
    |t::q -> aux_bornee q (insert_bornee ordre t accu)
  and init_accu l n =
    match l,n with
    |[], _ -> ([],[])
    |_,0 -> ([],l)
    |t::q,_ -> let (ac,qac) = init_accu q (n-1) in (insert ordre t ac,qac)  
in let (acc,q) = init_accu l n
in aux_bornee q acc

(* TESTS *)
let%test _ = npremier (fun x y -> x<y) 10 [] =[]
let%test _ = npremier (fun x y -> x<y) 10 [4;2;4;3;1] = [1;2;3;4;4]
let%test _ = npremier (fun x y -> x > y) 10 [4;7;2;4;1;2;2;7] = [7;7;4;4;2;2;2;1]
let%test _ = npremier (fun x y -> x > y) 1 [4;7;2;4;1;2;2;7]  = tronque 1 [7;7;4;4;2;2;2;1]
let%test _ = npremier (fun x y -> x > y) 2 [4;7;2;4;1;2;2;7]  = tronque 2 [7;7;4;4;2;2;2;1]
let%test _ = npremier (fun x y -> x > y) 3 [4;7;2;4;1;2;2;7]  = tronque 3 [7;7;4;4;2;2;2;1]
let%test _ = npremier (fun x y -> x > y) 4 [4;7;2;4;1;2;2;7]  = tronque 4 [7;7;4;4;2;2;2;1]
let%test _ = npremier (fun x y -> x > y) 5 [4;7;2;4;1;2;2;7]  = tronque 5 [7;7;4;4;2;2;2;1]
let%test _ = npremier (fun x y -> x > y) 6 [4;7;2;4;1;2;2;7]  = tronque 6 [7;7;4;4;2;2;2;1]
let%test _ =  npremier (fun x y -> x > y) 3 [4;7;2;4;1;2;2;7] = [7;7;4]
let%test _ =  npremier (fun x y -> x < y) 3 [4;7;2;4;1;2;2;7] = [1;2;2]

(* Initialize the accumulator with the sorted n first elements and then work with it. *)
(* The accumulator is in ascending order: the lowest one is first. Thus a new element either is disgarded (lower than the first), or the first is discarded and the new element is inserted at its place. *)
let npremier_opti ordre n l =
  let rec revordre x y = not (ordre x y)
  and splitnfirst n l = (* returns a couple (n first elements, tail) *)
    match n,l with
    | 0,_ -> ([], l)
    | _,[] -> ([], [])
    | _,a::l' -> let (x,y) = splitnfirst (n-1) l' in (a::x, y)
  and foo l acc =  (* the accumulator is in ascending order: the lowest one is first *)
    match l with
    | [] -> List.rev acc (* put back in descending order *)
    | a::l' ->
          let newacc = match acc with (* a replaces lowest element or is discarded *)
                       | b::acc' -> if (ordre a b) then (insert revordre a acc')
                                    else  acc
                       | _ -> acc (* impossible *)
        in foo l' newacc
  in let (initacc, tail) = (splitnfirst n l)
     in foo tail (tri_fusion revordre initacc)

let anneePrenomPlusDonne p (_s1,p1,_a1,nb1) (_s2,p2,_a2,nb2) =
  (p1=p && p2<>p)
  ||(p1=p && p2=p && nb1 > nb2)
  ||(p1<>p && p2<>p && nb1 > nb2)
  
let prenomPlusDonneAnnee a (_s1,_p1,a1,nb1) (_s2,_p2,a2,nb2) =
  (a1=a && a2<>a)
  ||(a1=a && a2=a && nb1 > nb2)
  ||(a1<>a && a2<>a && nb1 > nb2)


let debutBrut = Sys.time()
let%test_unit _ = List.iter print_stat (npremier_bete (anneePrenomPlusDonne "AURÉLIE")  10 listStat)
let%test_unit _ = List.iter print_stat (npremier_bete (prenomPlusDonneAnnee 1980)  10 listStat)
let finBrut = Sys.time()

let debutSubt = Sys.time()
let%test_unit _ = List.iter print_stat (npremier (anneePrenomPlusDonne "AURÉLIE")  10 listStat)
let%test_unit _ = List.iter print_stat (npremier (prenomPlusDonneAnnee 1980)  10 listStat)
let finSubt = Sys.time()

let debutOpti = Sys.time()
let%test_unit _ = List.iter print_stat (npremier_opti (anneePrenomPlusDonne "AURÉLIE")  10 listStat)
let%test_unit _ = List.iter print_stat (npremier_opti (prenomPlusDonneAnnee 1980)  10 listStat)
let finOpti = Sys.time()

let%test _ = (finSubt -. debutSubt) < (finBrut -. debutBrut)
let%test _ = Printf.printf "Brut=%f, Subst=%f, Opti=%f\n" (finBrut-.debutBrut) (finSubt-.debutSubt) (finOpti-.debutOpti); true
