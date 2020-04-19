val cumulative_apply: int -> (unit -> float) -> float
(** [cumulative_apply n f] applies f n times multiplicating their results *)

val cumulative_apply_i: int -> (int -> float) -> float
(** [cumulative_apply n f] applies f(i) n times multiplicating their results *)

val shuffle: 'a list -> 'a list
(** [shuffle l] shuffles list l *)