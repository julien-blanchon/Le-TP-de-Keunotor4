(* Flux et series de Taylor *)

type 'a option = 
	|None 
	| Some of 'a

module type Flux = 
sig 
	type 'a t val vide : 'a t 
	val cons : 'a -> 'a t -> 'a t 
	val uncons : 'a t -> ('a * 'a t) option 
	val apply : ('a -> 'b) t -> ('a t -> 'b t) 
	val recursion : ('a -> 'a) -> ('a -> 'a t) 
	val filter : ('a -> bool) -> 'a t -> 'a t
	val map : ('a -> 'b) -> 'a t -> 'b t 
	val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t 
end