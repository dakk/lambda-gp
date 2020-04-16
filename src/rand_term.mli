(** Generate a random variable between v random possible variables (bound to alphabet length) *)
val rand_var_bound: int -> string

(** Generate a random variable between v random possible variables *)
val rand_var: int -> string

(** Generate random lambda term with a deep of d using v free variables *)
val generate: int -> int -> Lambda.term

(** Generate random lambda term with a fixed length of l terms using v free variables *)
val generate_l: int -> int -> Lambda.term