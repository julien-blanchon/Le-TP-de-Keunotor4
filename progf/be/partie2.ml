(* Formules Propositionnelles *)

(* Exercice 8 *)
type formule =
  | Definition of string*formule*formule
  | Identifiant of string
  | Bool of bool
  | Et of formule * formule
  | Ou of formule * formule
  | Non of formule

type valuation = formule -> bool

(* Exercice 9 *)
let rec eval exp =
  let rec aux exp env =
    match exp with
    | Definition (i, def, e) -> aux e ((i,aux def env)::env)
    | Identifiant id -> List.assoc id env
    | Bool c -> c
    | Et (e1, e2) -> (aux e1 env) && (aux e2 env)
    | Ou (e1, e2) -> (aux e1 env) || (aux e2 env)
    | Non e1 -> not (aux e1 env)
  in aux exp []

(* Exercice 12 *)
module type Formule =
sig
  (* bool *)
  type t
  val const : bool -> t
  val et : t -> t -> t
  val ou : t -> t -> t
  val non : t -> t
end

type formule_bool = (module Formule with type t = bool)

type formule = formule_bool -> bool


module Valuation : Formule with type t = bool =
struct
  type t = bool
  let const c = c
  let et a b = a&&b
  let ou a b = a||b
  let non a = not a
end

Exercice 9 *)
let evaluer (f: formule) = f (module Valuation)
(*Par exemple on a bien:
   let e1 (type bool) (module F : Formule with type t=bool) = (F.const true)
   evaluer e1 = true
