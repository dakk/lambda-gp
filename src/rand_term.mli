val rand_var: int -> string
(** [rand_var v] returns a random variable from 0 to v *)

val generate: int -> int -> Lambda.term
(** [generate d v] generates a random term of depth d using v variables *)

val generate_l: int -> int -> Lambda.term
(** [generate_l l v] generates a random lambda term l long using v variables*)