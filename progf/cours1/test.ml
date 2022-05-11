let x = 32.0 +. 4.0;;
print_float x;;
print_endline "";;
let a = -3;;

let valeur_absolue = fun x -> if x>=0 then x else -x;;
let result = valeur_absolue(-2);;
print_int result;;
print_endline "";;

let b = if a>=0 then a else -a;;
print_int b;;

let divise x y = y mod x = 0;;

(** Divise **)
(* print divise 34 6;;  *)