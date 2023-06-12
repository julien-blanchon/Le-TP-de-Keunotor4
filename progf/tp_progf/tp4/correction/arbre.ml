open Assoc

type 'a arbre = Noeud of bool * ( ('a branche) list)
and 'a branche = 'a * 'a arbre

(* Pour les tests *)
let bb = ('b',Noeud(false,[('a',Noeud(false,[('s',Noeud(true,[]));('t',Noeud(true,[]))]))]))
let bd = ('d',Noeud(false,[('e',Noeud(true,[]))]))
let bl = ('l',Noeud(false,[('a',Noeud(true,[('i',Noeud(true,[('d',Noeud(true,[]));('t',Noeud(true,[]))]));('r',Noeud(false,[('d',Noeud(true,[]))]))]));
                           ('e',Noeud(true,[('s',Noeud(true,[]))]));
                           ('o',Noeud(false,[('n',Noeud(false,[('g',Noeud(true,[]))]))]))]))
let b1 = [bb;bd;bl]
let arbre_sujet = Noeud(false,b1)

(******************************************************************************)
(*   fonction d'appartenance d'une liste d'éléments à un arbre                *)
(*   signature  : appartient : 'a list -> 'a arbre -> bool                    *)
(*   paramètres : - une liste d'éléments (caractères dans le cas d'un dico)   *)
(*                - un arbre n-aire                                           *)
(*   résultat   : le résultat booléen du test                                 *)
(******************************************************************************)
let rec appartient_arbre lc (Noeud (b,lb)) =
  match lc with
  (* on a épuisé la liste : le résultat est le booléen du noeud sur
     lequel on est arrivé *)
  | [] -> b
  (* sinon on cherche la branche correspondant au premier
     caractère de la liste :
     - elle n'existe pas : le mot n'appartient pas au trie
     - on la trouve, on relance aux avec le reste de la liste
     et l'arbre de cette branche *)
  | c::qlc ->
     match recherche c lb with
     | None -> false
     | Some a -> appartient_arbre qlc a

let%test _ = appartient_arbre ['b';'a';'s']  arbre_sujet
let%test _ = appartient_arbre ['b';'a';'t']  arbre_sujet
let%test _ = appartient_arbre ['d';'e']  arbre_sujet
let%test _ = appartient_arbre ['l';'a']  arbre_sujet
let%test _ = appartient_arbre ['l';'a';'i']  arbre_sujet
let%test _ = appartient_arbre ['l';'a';'i';'d']  arbre_sujet
let%test _ = appartient_arbre ['l';'a';'i';'t']  arbre_sujet
let%test _ = appartient_arbre ['l';'a';'r';'d']  arbre_sujet
let%test _ = appartient_arbre ['l';'e']  arbre_sujet
let%test _ = appartient_arbre ['l';'e';'s']  arbre_sujet
let%test _ = appartient_arbre ['l';'o';'n';'g']  arbre_sujet
let%test _ = not (appartient_arbre ['t';'o';'t';'o'] arbre_sujet)
let%test _ = not (appartient_arbre ['b';'a']  arbre_sujet)
let%test _ = not (appartient_arbre ['l';'o';'n']  arbre_sujet)

(******************************************************************************)
(*   fonction d'ajout d'une liste éléments dans un arbre                      *)
(*   signature  : ajout : 'a list -> 'a arbre -> 'a arbre                     *)
(*   paramètres : - une liste d'éléments (caractères dans le cas d'un dico)   *)
(*                - un arbre n-aire                                           *)
(*   résultat   : l'arbre n-aire avec le mot ajouté                           *)
(******************************************************************************)
let rec ajout_arbre lc (Noeud (b, lb)) =
  match lc with
  (* on a épuisé la liste : le résultat est le noeud sur lequel on
     est arrivé avec son booléen mis à vrai *)
  | [] -> Noeud (true, lb)
  (* sinon on cherche l'arbre arbre_c de la branche correspondant
     au premier caractère de la liste;
     si on ne le trouve pas, le résultat de cette recherche est un arbre
     avec une liste de branches vide.

     Le résultat de aux est le noeud en paramètre
     que l'on met à jour en remplacant dans sa liste de branches,
     la branche du premier caractère par la branche dont l'arbre est
     le résultat de l'ajout du reste des caractères à l'arbre arbre_c *)
  | c::qlc ->
     let arbre_c =
       let l = recherche c lb in
       match l with
       | None   -> Noeud (false, [])
       | Some a -> a
     in Noeud (b, maj c (ajout_arbre qlc arbre_c) lb)

let arbre_sujet2 =
  List.fold_right ajout_arbre
    [['b';'a';'s']; ['b';'a';'t']; ['d';'e']; ['l';'a']; ['l';'a';'i'];
     ['l';'a';'i';'d']; ['l';'a';'i';'t']; ['l';'a';'r';'d']; ['l';'e'];
     ['l';'e';'s']; ['l';'o';'n';'g']]
    (Noeud (false,[]))

let arbre_sujet3 =
  List.fold_right ajout_arbre
    [['b';'a';'s']; ['l';'a';'i';'t']; ['b';'a';'t']; ['l';'e']; ['d';'e'];
     ['l';'a';'i']; ['l';'a';'i';'d']; ['l';'e';'s']; ['l';'a';'r';'d'];
     ['l';'a']; ['l';'o';'n';'g']]
    (Noeud (false,[]))

let%test _ = arbre_sujet2 = arbre_sujet
let%test _ = arbre_sujet3 = arbre_sujet

(*  Retrait **)
let rec retrait_arbre lc (Noeud (b, lb)) =
  match lc with
  (* même principe que pour la recherche et l'ajout *)
  | [] -> Noeud (false, lb)
  | c::qlc ->
     match recherche c lb with
     | None -> Noeud (b, lb)
     | Some a -> Noeud(b, maj c (retrait_arbre qlc a) lb)

(*  Parcours de l'arbre **)
(* fonction auxiliaire qui travaille sur une branche *)
let rec dico_branche (c, Noeud(b,lf)) =
  List.map (fun e -> c::e ) (parcours (Noeud(b,lf))) 
(* fonction principale qui travaille sur l'arbre *)
and parcours (Noeud(b,lf)) =
  let liste_fils = List.flatten (List.map dico_branche  lf) in
  if (b) then []::liste_fils else liste_fils


let%test _ = parcours arbre_sujet = [['b';'a';'s']; ['b';'a';'t']; ['d';'e']; ['l';'a']; ['l';'a';'i'];
['l';'a';'i';'d']; ['l';'a';'i';'t']; ['l';'a';'r';'d']; ['l';'e'];
['l';'e';'s']; ['l';'o';'n';'g']]

(* Autre version *)

let parcours2 (Noeud(b,lf)) =
  let rec dico lf =
    match lf with
    | [] -> []
    | (c, Noeud (b, lff))::qlf ->
       (* on calcule la liste des listes de caractères des fils du
          premier noeud *)
       let llc = dico lff in
       (* si le noeud contient vrai, le caractère de la branche sera
          le dernier caractère d'un mot :
          on le rajoute en tête de la liste des listes de caractères
          des fils auxquelles on met au début le caractère
          et on concatène le tout avec le résultat de dico
          sur la queue de la liste des fils *)
       if b then ([c]::(List.map (function a -> c::a) llc))@(dico qlf)
       (* sinon on ne fait que rajouter ce caractère en tête
          de toutes les listes et on continue avec la queue de
          la liste *)
       else (List.map (function a -> c::a) llc)@(dico qlf)
  in dico lf

  let%test _ = parcours2 arbre_sujet = [['b';'a';'s']; ['b';'a';'t']; ['d';'e']; ['l';'a']; ['l';'a';'i'];
  ['l';'a';'i';'d']; ['l';'a';'i';'t']; ['l';'a';'r';'d']; ['l';'e'];
  ['l';'e';'s']; ['l';'o';'n';'g']]

(*  Normalisation ***)
let rec normalisation_arbre (Noeud(b,branches)) =
  let lwithvide = List.map (fun (c,t) -> (c,normalisation_arbre t)) branches in
  let lwithoutvide =
    List.fold_right (fun t tq ->
        match t with
        | (_,(Noeud(false,[]))) ->tq
        |_ ->  t::tq)
      lwithvide [] in
    Noeud(b,lwithoutvide)

let rec normalisation_arbre2 (Noeud(b,branches)) = Noeud(b, parcours_branches branches)
and parcours_branches branches =
  match branches with
  | [] -> []
  | (c,t)::q ->
     match normalisation_arbre2 t with
     | Noeud(false, []) -> parcours_branches q
     | tt               -> (c,tt):: (parcours_branches q)

(* TESTS *)

let arbre_sujet_une_seule_branche  = List.fold_right retrait_arbre  [['b';'a';'s']; ['b';'a';'t']; ['d';'e']] arbre_sujet
let%test _ = normalisation_arbre2 arbre_sujet_une_seule_branche = normalisation_arbre arbre_sujet_une_seule_branche

let rec retrait_normalise_arbre lc (Noeud (b, lb)) =
  match lc with
  (* même principe que pour la recherche et l'ajout *)
  | [] -> Noeud (false, lb)
  | c::qlc ->
     match recherche c lb with
     | None -> Noeud (b, lb)
     | Some a ->
        let bc = retrait_normalise_arbre qlc a in
        match bc with
        (* on ne garde pas la branche si elle mène à un noeud mort *)
        | Noeud(false, []) -> Noeud(b, List.remove_assoc c lb)
        | _ -> Noeud(b, maj c bc lb)

(* TESTS *)

let arbre_sujet_une_seule_branche_norm  = List.fold_right retrait_normalise_arbre  [['b';'a';'s']; ['b';'a';'t']; ['d';'e']] arbre_sujet
let%test _ = normalisation_arbre2 arbre_sujet_une_seule_branche = arbre_sujet_une_seule_branche_norm
