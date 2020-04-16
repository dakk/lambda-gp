type term =
	| Var of string 
	| Abs of (string * term) 
	| App of term * term

val to_string: term -> string
val reduce_fix: term -> term
val reduce: term -> int -> term
val subst: string -> term -> term -> term