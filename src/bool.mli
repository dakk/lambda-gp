exception NotABoolTerm

val ltrue: Lambda.term
val lfalse: Lambda.term
val to_bool: Lambda.term -> bool
val of_bool: bool -> Lambda.term
val is_bool: Lambda.term -> bool