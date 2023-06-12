(* Exercice 1 *)
type 'a set = 'a list

(* Exercice 2 *)
(*CONTRAT
Fonction qui ajoute un élément dans une ensemble
Type : 'a -> 'a set -> 'a set
Paramètre : element, l'élement à ajouter
Paramètre : set, l'ensemble dans lequel ajouter element
Résultat : l'ensemble avec les éléments de set et element
*)
(* let ajouter element set = element::set *)
let rec ajouter element set =
  match set with
  | [] -> [element]
  | [e] -> [e; element]
  | t::q -> if (t!=element)
           then t::(ajouter element q)
           else t::q

let%test _ = ajouter 1 [1;2;3;4] = [1;2;3;4]
let%test _ = ajouter 10 [1;2;3;4] = [1;2;3;4;10]

(*CONTRAT
Fonction qui recherche un élément dans ensemble
Type : 'a -> 'a set -> 'a set
Paramètre : element, l'élément à rechercher
Paramètre : set, l'ensemble dans lequel rechercher element
Résultat : true si l'element est présent dans set, false sinon
*)
let rechercher element set = List.mem element set

let%test _ = rechercher 1 [1;2;3;4] = true
let%test _ = rechercher 10 [1;2;3;4] = false

(*CONTRAT
Fonction qui retire un élément d'un ensemble
Type : 'a -> 'a set -> 'a set
Paramètre : element, l'élément à retirer
Paramètre : set, l'ensemble dans lequel retirer element
Résultat : l'ensemble avec les éléments de set mais sans element
*)
let retirer element set = List.filter (fun a -> not(a=element)) set

let%test _ = retirer 1 [1;2;3;4] = [2;3;4]
let%test _ = retirer 10 [1;2;3;4] = [1;2;3;4]

(* Exercice 3 *)
(*CONTRAT
Fonction qui filtre les élément d'un ensemble
Type : ('a -> bool) -> 'a set -> 'a set
Paramètre : condition, condition de filtrage
Paramètre : set, l'ensemble dans lequel filtrer
Résultat : l'ensemble filtrer
*)
let filter condition set = List.filter condition set

let%test _ = filter (fun a -> ((a mod 2) =0)) [1;2;3;4] = [2;4]

(* Exercice 4 *)
type 'a graph = Graph of (('a set) * ('a*'a) set)

(* Exercice 5 *)
let ajouter_noeud n (Graph(noeuds, arretes)) = 
  Graph(
    ajouter n noeuds,
    arretes
  )

let retirer_noeud n (Graph(noeuds, arretes)) = 
  Graph(
    retirer n noeuds,
    filter (fun (a,b) -> a!=n && b!=n) arretes
  )

(* Exercice 6 *)
type 'a jeu = ((bool)*(bool)*('a)*(int)) graph
(*********************win1, win2, state, to_play**********)

(* type noeud_jeu = (bool*bool*int*int)
type graph_jeu = noeud_jeu graph
type init_jeu = noeud_jeu
type jeu = init_jeu*graph_jeu *)

(* Exercice 7 *)
let rec chemin_possible graph_jeu chemin =
  match chemin with
    | [] -> true
    | [e] -> rechercher e graph_jeu
    | (s11, s12)::(s21, s22)::q -> 
      (s12=s21) &&
      (rechercher (s11, s12) graph_jeu) &&
      (rechercher (s21, s22) graph_jeu) &&
      (chemin_possible graph_jeu q)


let state14_jeu1 = (false, false, 1, 4)
let state22_jeu1 = (false, false, 2, 2)
let state23_jeu1 = (false, false, 2, 3)
let state10_jeu1 = (true, false, 1, 0)
let state11_jeu1 = (false, false, 1, 1)
let state12_jeu1 = (false, false, 1, 2)
let state20_jeu1 = (true, false, 2, 0)
let state21_jeu1 = (false, false, 2, 1)


let graph_jeu1 = Graph(
  [state14_jeu1; state14_jeu1; state22_jeu1; state23_jeu1; state10_jeu1; state11_jeu1; state12_jeu1; state20_jeu1; state21_jeu1],
  [
    (state14_jeu1, state22_jeu1); (state14_jeu1, state23_jeu1);
    (state22_jeu1, state10_jeu1); (state22_jeu1, state11_jeu1);
    (state23_jeu1, state11_jeu1); (state23_jeu1, state12_jeu1);
    (state23_jeu1, state11_jeu1); (state23_jeu1, state12_jeu1);
    (state11_jeu1, state20_jeu1);
    (state12_jeu1, state20_jeu1); (state12_jeu1, state21_jeu1);
    (state21_jeu1, state10_jeu1);
  ]
)

let chemin1 = [
  (state14_jeu1, state22_jeu1); (state14_jeu1, state23_jeu1);
  (state22_jeu1, state10_jeu1); (state22_jeu1, state11_jeu1);
]

let chemin2 = [
  (state14_jeu1, state22_jeu1); (state22_jeu1, state11_jeu1);
  (state11_jeu1, state20_jeu1)
]

let cc = [
    (state14_jeu1, state22_jeu1); (state14_jeu1, state23_jeu1);
    (state22_jeu1, state10_jeu1); (state22_jeu1, state11_jeu1);
    (state23_jeu1, state11_jeu1); (state23_jeu1, state12_jeu1);
    (state23_jeu1, state11_jeu1); (state23_jeu1, state12_jeu1);
    (state11_jeu1, state20_jeu1);
    (state12_jeu1, state20_jeu1); (state12_jeu1, state21_jeu1);
    (state21_jeu1, state10_jeu1);
  ]

let%test _ = chemin_possible cc chemin1 = false
let%test _ = chemin_possible cc chemin2 = true