exception NotAChurchTerm

val church: int -> Lambda.term
val unchurch: Lambda.term -> int
val is_church: Lambda.term -> bool