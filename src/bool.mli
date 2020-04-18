exception NotABoolTerm
(** raised whenever trying to convert a non boolean term to boolean *)

val ltrue: Lambda.term
(** true lambda term *)

val lfalse: Lambda.term
(** false lambda term *)

val to_bool: Lambda.term -> bool
(** [to_bool t] converts a lambda term t to boolean *)

val of_bool: bool -> Lambda.term
(** [of_bool b] converts a boolean t to a lambda term *)

val is_bool: Lambda.term -> bool
(** [is_bool t] returns true if t is a boolean lambda term *)