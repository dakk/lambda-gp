type fitness_f = Lambda.term -> float
type valid_f = Lambda.term -> bool

type ga_settings = {
  fitness_f: fitness_f;
  valid_f: valid_f;
  fitness_target: float;
  pop_size: int;
  term_len: int;
  var_n: int;
  gen_n: int;
}

type ga_state = {
  settings: ga_settings;
  population: (Lambda.term * float) list;
  avg_fitness: float;
  best_fitness: float;
  generation: int;
}

(* type population = Lambda.term list

(** Crossover a pair of terms *)
val crossover: (Lambda.term * Lambda.term) -> (Lambda.term * Lambda.term)

(** Crossover a pair of terms, k times *)
val crossover_k: int -> (Lambda.term * Lambda.term) -> (Lambda.term * Lambda.term)

(** Mutate a term *)
val mutate: Lambda.term -> Lambda.term
*)

(** Evalute fitness value for a given term *)
(* val fitness: fitness_f -> Lambda.term -> float *)


val ga_init: ga_settings -> ga_state
val ga_print: ga_state -> unit
val log: ('a -> 'b, out_channel, unit, unit, unit, unit) format6 -> 'a -> 'b