open Assoc
open Arbre
open Chaines

(* petit lexique des identificateurs :

   comme je n'ai pas envie de perdre trop de temps à chercher
   et à taper les identificateurs, voici quelques explications
   (cette démarche est cependant à éviter pour vos projets !)

   s : une chaîne de "caractères"
   c, c', car : un "caractère"
   lc : une liste de "caractères"
   llc : une liste de listes de "caractères"
   b, b' : un booléen
   a : un arbre
   lb, lb' : une liste de branches
   fd, fr : les fonctions de décomposition/recomposition
*)

(* le type trie :
    triplet arbre,
            fonction de décomposition mot -> liste de caractères,
            fonction de recomposition liste de caractères -> mot *)
type ('a,'b) trie = Trie of ('b arbre) * ('a -> 'b list) * ('b list -> 'a)

(* Pour les tests *)
let bb = ('b',Noeud(false,[('a',Noeud(false,[('s',Noeud(true,[]));('t',Noeud(true,[]))]))]))
let bd = ('d',Noeud(false,[('e',Noeud(true,[]))]))
let bl = ('l',Noeud(false,[('e',Noeud(true,[('s',Noeud(true,[]))]))]))
let b1 = [bb;bd;bl]
let arbre_sujet = Noeud(false,b1)

(******************************************************************************)
(*   fonction de création d'un nouveau trie                                   *)
(*   signature  : nouveau :                                                   *)
(*          ('a -> 'b list) -> ('b list -> 'a) -> ('a, 'b) trie = <fun>       *)
(*   paramètres : - une fonction de décomposition                             *)
(*                     mot -> liste de caractères                             *)
(*                -  une fonction de recomposition                            *)
(*                     liste de caractères -> mot                             *)
(*   résultat     : un nouveau trie "vide"                                    *)
(******************************************************************************)
let nouveau fd fr = Trie ( Noeud (false, []), fd, fr)

(******************************************************************************)
(*   fonction d'appartenance d'un élément à un trie                           *)
(*   signature  : appartient : 'a -> ('a, 'b) trie -> bool = <fun>            *)
(*   paramètres : - un mot                                                    *)
(*                - un trie                                                   *)
(*   résultat   : le résultat booléen du test                                 *)
(******************************************************************************)
let appartient mot (Trie (a,decomp,comp)) =
  let lc = decomp mot in
  appartient_arbre lc a

(******************************************************************************)
(*   fonction d'ajout d'un élément dans un trie                               *)
(*   signature  : ajout : 'a -> ('a, 'b) trie -> ('a, 'b) trie = <fun>        *)
(*   paramètres : - un mot                                                    *)
(*                - un trie                                                   *)
(*   résultat   : le trie avec le mot ajouté                                  *)
(******************************************************************************)
let ajout mot (Trie (a,decomp,comp)) =
  let lc = decomp mot in
  Trie (ajout_arbre lc a,decomp,comp)

(* Tests appartient et ajout *)

let decomp = (fun x-> x)
let recomp = (fun x -> x)

let%test _ =
  let a0 = nouveau decomp recomp in
  let a1 = ajout ['d';'e'] a0 in
  let (Trie(arbre,decomp,recomp))=a1 in
  arbre = Noeud(false,[bd])

let%test _ =
  let a0 = nouveau decomp recomp in
  let a1 = ajout ['d';'e'] a0 in
  let a2 = ajout ['b';'a';'s'] a1 in
  let a3 = ajout ['b';'a';'t'] a2 in
  let (Trie(arbre,decomp,recomp))=a3 in
  arbre = Noeud(false,[bb;bd])

let%test _ =
  let a0 = nouveau decomp recomp in
  let a1 = ajout ['d';'e'] a0 in
  let a2 = ajout ['b';'a';'s'] a1 in
  let a3 = ajout ['b';'a';'t'] a2 in
  let a4 = ajout ['l';'e';'s'] a3 in
  let a5 = ajout ['l';'e'] a4 in
  let (Trie(arbre,decomp,recomp))=a5 in
  arbre = arbre_sujet

let%test _ =
  let a0 = nouveau decomp recomp in
  let a1 = ajout ['d';'e'] a0 in
  let a2 = ajout ['b';'a';'s'] a1 in
  let a3 = ajout ['b';'a';'t'] a2 in
  let a4 = ajout ['l';'e';'s'] a3 in
  let a5 = ajout ['l';'e'] a4 in
  appartient ['l';'e';'s'] a5

let%test _ =
  let a0 = nouveau decomp recomp in
  let a1 = ajout ['d';'e'] a0 in
  let a2 = ajout ['b';'a';'s'] a1 in
  let a3 = ajout ['b';'a';'t'] a2 in
  let a4 = ajout ['l';'e';'s'] a3 in
  let a5 = ajout ['l';'e'] a4 in
  appartient ['l';'e'] a5

let%test _ =
  let a0 = nouveau decomp recomp in
  let a1 = ajout ['d';'e'] a0 in
  let a2 = ajout ['b';'a';'s'] a1 in
  let a3 = ajout ['b';'a';'t'] a2 in
  let a4 = ajout ['l';'e';'s'] a3 in
  let a5 = ajout ['l';'e'] a4 in
  appartient ['d';'e'] a5

let%test _ =
  let a0 = nouveau decomp recomp in
  let a1 = ajout ['d';'e'] a0 in
  let a2 = ajout ['b';'a';'s'] a1 in
  let a3 = ajout ['b';'a';'t'] a2 in
  let a4 = ajout ['l';'e';'s'] a3 in
  let a5 = ajout ['l';'e'] a4 in
  appartient ['b';'a';'s'] a5

let%test _ =
  let a0 = nouveau decomp recomp in
  let a1 = ajout ['d';'e'] a0 in
  let a2 = ajout ['b';'a';'s'] a1 in
  let a3 = ajout ['b';'a';'t'] a2 in
  let a4 = ajout ['l';'e';'s'] a3 in
  let a5 = ajout ['l';'e'] a4 in
  not (appartient ['b';'a'] a5)

let%test _ =
  let a0 = nouveau decomp recomp in
  let a1 = ajout ['d';'e'] a0 in
  let a2 = ajout ['b';'a';'s'] a1 in
  let a3 = ajout ['b';'a';'t'] a2 in
  let a4 = ajout ['l';'e';'s'] a3 in
  let a5 = ajout ['l';'e'] a4 in
  not (appartient ['c';'h';'i';'e';'n'] a5)

(******************************************************************************)
(*   fonction de retrait d'un élément d'un trie                               *)
(*   signature  : trie_retrait : 'a -> ('a, 'b) trie -> ('a, 'b) trie = <fun> *)
(*   paramètres : - un mot                                                    *)
(*                - un trie                                                   *)
(*   résultat   : le trie avec le mot retiré                                  *)
(******************************************************************************)
let retrait mot (Trie(a, fd, fr)) =
   Trie(retrait_arbre (fd mot) a, fd , fr)

let%test _ =
  let a0 = nouveau decomp recomp in
  let a1 = ajout ['d';'e'] a0 in
  let a2 = ajout ['b';'a';'s'] a1 in
  let a3 = ajout ['b';'a';'t'] a2 in
  let a4 = ajout ['l';'e';'s'] a3 in
  let a5 = ajout ['l';'e'] a4 in
  not (appartient  ['l';'e'] (retrait ['l';'e'] a5))

let%test _ =
  let a0 = nouveau decomp recomp in
  let a1 = ajout ['d';'e'] a0 in
  let a2 = ajout ['b';'a';'s'] a1 in
  let a3 = ajout ['b';'a';'t'] a2 in
  let a4 = ajout ['l';'e';'s'] a3 in
  let a5 = ajout ['l';'e'] a4 in
  appartient  ['l';'e'] (retrait ['l';'e';'s'] a5)

let%test _ =
  let a0 = nouveau decomp recomp in
  let a1 = ajout ['d';'e'] a0 in
  let a2 = ajout ['b';'a';'s'] a1 in
  let a3 = ajout ['b';'a';'t'] a2 in
  let a4 = ajout ['l';'e';'s'] a3 in
  let a5 = ajout ['l';'e'] a4 in
  not (appartient  ['l';'e';'s'] (retrait ['l';'e';'s'] a5))

let%test _ =
  let a0 = nouveau decomp recomp in
  let a1 = ajout ['d';'e'] a0 in
  let a2 = ajout ['b';'a';'s'] a1 in
  let a3 = ajout ['b';'a';'t'] a2 in
  let a4 = ajout ['l';'e';'s'] a3 in
  let a5 = ajout ['l';'e'] a4 in
  appartient  ['l';'e';'s'] (retrait ['l';'e'] a5)

(******************************************************************************)
(*   fonction interne au Module qui génère la liste de tous les mots          *)
(*   d'un trie                                                                *)
(*   signature    : trie_dico : ('a, 'b) trie -> 'a list = <fun>              *)
(*   paramètre(s) : le trie                                                   *)
(*   résultat     : la liste des mots                                         *)
(******************************************************************************)
let trie_dico (Trie(a,fd,fr)) =
  (* une fois qu'on a la liste de listes de caractères, on applique
     la fonct ion de recomposition à toutes les listes pour former
     les mots *)
  List.map fr (Arbre.parcours a)

(* Autre version *)
let trie_dico2 (Trie(a,fd,fr)) =
  List.map fr (Arbre.parcours2 a)

(******************************************************************************)
(* procédure d'affichage d'un trie                                            *)
(*   signature  : affiche :  ('a -> unit) -> ('a, 'b) trie -> unit = <fun>    *)
(*   paramètres : - une procédure d'affichage d'un mot                        *)
(*                - un trie                                                   *)
(*   résultat   : aucun                                                       *)
(******************************************************************************)
let affiche (p : 'a -> unit) (t : ('a, 'b) trie) =
  List.iter p (trie_dico t)

(**** BONUS ****)

(* Exercice 8 *)
(* Fonction de retrait normalisé *)
(* Type : 'a -> ('a * 'b) arbre_lex ->  ('a * 'b) arbre_lex *)
let retrait_normalise mot (Trie(a, fd, fr)) =
  Trie(normalisation_arbre (retrait_arbre (fd mot) a), fd , fr)
(* ATTENTION : tel que fait, il y a plusieurs parcours...*)
(* On pourrait faire la normalisation en même temps que le retrait pour être plus efficace *)

(*  Pour les tests *)
let trie_sujet =
  List.fold_right ajout
    ["bas"; "bât"; "de"; "la"; "lai"; "laid"; "lait"; "lard"; "le"; "les"; "long"]
    (nouveau decompose_chaine recompose_chaine)
