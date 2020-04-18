type term =
	| Var of string 
	| Abs of (string * term) 
	| App of term * term
(** lambda term type definition *)

val get_var: int -> string
(** [get_var i] returns a var identified by its index i *)

type 'a set = Set of 'a list
(** set used for free variables *)

val fv: term -> string set
(** [fv t] returns free variables of t *)

val fv_l: term -> string list
(** [fv_l t] returns free variables of t as a list *)

val to_string: term -> string
(** [to_string t] converts a term to its string representation *)

(* exception InvalidLambdaString *)
(* val parse: string -> term *)
(** [parse s] converts s to a term, or raise InvalidLambdaString on failures *)

val reduce_fix: term -> term
val reduce_fix_timeout: ?n:int -> term -> term
val reduce: int -> term -> term
val has_redex: term -> bool
val subst: string -> term -> term -> term

val len: term -> int
(** [len t] returns the length of t *)