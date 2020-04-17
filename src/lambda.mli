(* exception InvalidLambdaString *)

type term =
	| Var of string 
	| Abs of (string * term) 
	| App of term * term

type 'a set = Set of 'a list;;

val get_var: int -> string

val fv: term -> string set
val fv_l: term -> string list

val to_string: term -> string
(* val parse: string -> term *)

val reduce_fix: term -> term
val reduce: int -> term -> term
val has_redex: term -> bool
val subst: string -> term -> term -> term

(** Return the length of a term *)
val len: term -> int