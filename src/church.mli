exception NotAChurchTerm

val of_int: int -> Lambda.term
val to_int: Lambda.term -> int
val is_church: Lambda.term -> bool
val to_int2: Lambda.term -> int
val is_church2: Lambda.term -> bool

(** Try to convert a term using different variables to a canonical church term *)
val convert_to_canonical: Lambda.term -> Lambda.term