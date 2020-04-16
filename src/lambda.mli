type term =
	| Var of string 
	| Abs of (string * term) 
	| App of term * term

val to_string: term -> string
val reduce_fix: term -> term
val reduce: int -> term -> term
val has_redex: term -> bool
val subst: string -> term -> term -> term

(** Return the length of a term *)
val len: term -> int