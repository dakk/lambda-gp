exception NotAChurchTerm
(** raised whenever a to_int conversion fails *)

val of_int: int -> Lambda.term
(** [of_int n] converts n to lambda term *)

val to_int: Lambda.term -> int
(** [to_int t] converts encoded number t to integer *)

val is_church: Lambda.term -> bool
(** [is_church t] returns true if t is an encoded number *)

val convert_to_church: Lambda.term -> Lambda.term
(** [convert_to_church t] tries to convert a term t using different variables to a church term *)

