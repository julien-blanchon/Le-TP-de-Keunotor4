(*****************************************************************
parties : ’a list -> ’a list list
argument: l, une liste quelconque d’éléments
résultat: une liste de listes, dont les 2^(List.length l) éléments
sont les sous-listes de l. Si les élements de l sont
différents, les listes éléments du résultat seront
différentes aussi, et formeront les parties de l.
*****************************************************************)

(* Exercice 1: Donner une formulation récursive comptant le nombre de parties d’un ensemble de cardinal n  *)

(* nombre_de_partie(e) = 2*nombre_de_partie(tete) (=2^n)*)
let rec nombre_de_partie e =
  match e with 
    | [] -> 1
    | queue::tete -> 2*nombre_de_partie(tete)

let%test _ = nombre_de_partie [] = 1
let%test _ = nombre_de_partie [1;2;3] = 8


(* Exercice 2: — Écrire la fonction ajout, qui à partir d’un élément e et d’ensembles {E1, . . . , En} renvoie l’ensemble
{E1, {e} ∪ E1, . . . , En, {e} ∪ En}. *)

let ajout e e_multiple = e_multiple @ (List.map (fun e_simple -> e::e_simple) e_multiple)

let rec parties e_simple = 
  match e_simple with
    | [] -> [[]]
    | queue::tete -> ajout queue (parties tete)


(* 2 Permutations d’une liste *)

(* ****************************************************************
permutations : ’a list -> ’a list list
argument: l, une liste quelconque d’éléments
résultat: une liste de listes, dont les (List.length l)! éléments
ont même longueur que l. Si les élements de l sont
différents, les listes éléments du résultat seront
différentes aussi, et formeront les permutations de l.
**************************************************************** *)

let rec insertion e l = match l with
  | [] -> [[e]]
  | [r] -> [[e;r]; [r;e]] (*pas forcement utile !*)
  | t::q -> (e::l)::List.map(fun ll->t::ll)(insertion e q)



let rec permutations l = match l with
  | [] -> []
  | [r] -> [[r]]
  | t::q -> List.flatten(List.map(fun ll -> insertion t ll)(permutations q))
(*C'est n'importe quoi 😅*)

(* 3 Combinaisons *)

(* ****************************************************************
combinaisons : ’a list -> int -> ’a list list
argument: l, une liste quelconque d’éléments supposés différents
k, le nombre d’élements distincts à tirer
résultat: une liste de combinaisons. Chaque combinaison est elle-même
une liste d’éléments, dont les éléments
sont ceux de l.
**************************************************************** *)

(* Donner une formulation récursive comptant le nombre de combinaisons de k éléments d’un
ensemble à n éléments. *)

(* nbr_combinaison(n, k) == nbr_combinaison(n-1, k) * nbr_combinaison(n-1, k-1) *)

(* Écrire la fonction combinaisons (contrat+code+test) *)

let rec combinaison ensemble k = 
  let n = List.length ensemble in
    match k with
      | 0 -> [[]]
      | n -> [ensemble]
      | _ -> match ensemble with
        | [] -> [[]]
        | tete::queue -> (combinaison queue k) @ (ajout tete (combinaison queue (k-1)))