exception NotAChurchTerm

val of_int: int -> Lambda.term
val to_int: Lambda.term -> int
val is_church: Lambda.term -> bool
val to_int2: Lambda.term -> int
val is_church2: Lambda.term -> bool

val convert_to_church: Lambda.term -> Lambda.term
(** [convert_to_church t] tries to convert a term t using different variables to a church term *)