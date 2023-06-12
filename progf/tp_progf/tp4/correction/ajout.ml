(* le type arbre n-aire avec le "caractère" dans la branche *)
type 'a arbre = Noeud of bool * ( ('a * 'a arbre) list)

(* le type trie :
    triplet arbre,
            fonction de décomposition mot -> liste de caractères,
            fonction de recomposition liste de caractères -> mot *)
type ('a,'b) trie = Trie of 'b arbre * ('a -> 'b list) * ('b list -> 'a)

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

(* Pour les tests *)
let bb = ('b',Noeud(false,[('a',Noeud(false,[('s',Noeud(true,[]));('t',Noeud(true,[]))]))]))
let bd = ('d',Noeud(false,[('e',Noeud(true,[]))]))
let bl = ('l',Noeud(false,[('e',Noeud(true,[('s',Noeud(true,[]))]))]))
let b1 = [bb;bd;bl]
let arbre_sujet = Noeud(false,b1)

(******************************************************************************)
(*   fonction interne au Module de recherche dans une liste                   *)
(*   de la branche correspondant à un caractère                               *)
(*   signature  : recherche :                                                 *)
(*                  'a -> ('a * 'b) list -> ('a * 'b) option = <fun>          *)
(*   paramètres : - un caractère                                              *)
(*                - une liste de branches                                     *)
(*   résultat     : Some (la branche correspondant au caractère),             *)
(*                  si elle existe                                            *)
(*                  None, sinon                                               *)
(******************************************************************************)
let rec recherche c lb =
  match lb with
  | []            -> None
  | (tc, ta)::qlb ->
     if c < tc then None
     else if c = tc then Some ta
     else recherche c qlb

(* TEST *)
let%test _ = recherche 'b' b1 = Some bb
let%test _ = recherche 'd' b1 = Some bd
let%test _ = recherche 'l' b1 = Some bl
let%test _ = recherche 'a' b1 = None

(******************************************************************************)
(*   fonction interne au Module d'ajout/mise à jour du fils d'un arbre        *)
(*   signature  : maj :                                                       *)
(*                 'a -> 'a * 'b -> ('a * 'b) list -> ('a * 'b) list = <fun>  *)
(*   paramètres : - un caractère                                              *)
(*                - la branche à ajouter/modifier                             *)
(*                - la liste de branches                                      *)
(*   résultat   : la liste de branches mise à jour                            *)
(******************************************************************************)
let rec maj c nouvelle_b lb =
  match lb with
  | [] -> [(c,nouvelle_b)]
  | (tc, ta)::qlb ->
     if c < tc then (c,nouvelle_b)::lb
     else if c = tc then (c,nouvelle_b)::qlb
     else (tc, ta)::(maj c nouvelle_b qlb)

(* TESTS *)

let%test _ = maj 'b' bl b1 = [bl;bd;bl]
let ba = ('a',Noeud(true,[('n',Noeud(true,[]));('u',Noeud(true,[]))]))
let%test _ = maj 'a' ba b1 = [ba;bb;bd;bl]
let bm = ('m',Noeud(false,[('a',Noeud(true,[]))]))
let%test _ = maj 'm' bm b1 = [bb;bd;bl;bm]

(******************************************************************************)
(*   fonction d'appartenance d'un élément à un trie                           *)
(*   signature  : appartient : 'a -> ('a, 'b) trie -> bool = <fun>            *)
(*   paramètres : - un mot                                                    *)
(*                - un trie                                                   *)
(*   résultat   : le résultat booléen du test                                 *)
(******************************************************************************)
let appartient mot (Trie(a, fd, _)) =
  (* la fonction auxiliaire travaille sur la liste de caractères *)
  let rec aux lc (Noeud (b,lb)) =
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
       | Some (c',a) -> aux qlc a
  in aux (fd mot) a

(******************************************************************************)
(*   fonction d'ajout d'un élément dans un trie                               *)
(*   signature  : ajout : 'a -> ('a, 'b) trie -> ('a, 'b) trie = <fun>        *)
(*   paramètres : - un mot                                                    *)
(*                - un trie                                                   *)
(*   résultat   : le trie avec le mot ajouté                                  *)
(******************************************************************************)
let ajout mot (Trie(a, fd, fr)) =
  (* la fonction auxiliaire travaille sur la liste de caractères *)
  let rec aux lc (Noeud (b, lb)) =
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
         | Some (c', a) -> a
       in Noeud(b, maj c (c, (aux qlc arbre_c)) lb)
  in Trie(aux (fd mot) a, fd , fr)

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
