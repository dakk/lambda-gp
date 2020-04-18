type fitness_f = Lambda.term -> float
type valid_f = Lambda.term -> bool
type test_best_f = Lambda.term -> bool

type ga_settings = {
  test_best_f: test_best_f;
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
  avg_term_len: int;
  best_fitness: float;
  generation: int;
}

val ga_init: ga_settings -> ga_state
(** [ga_init s] initializes the ga using settings s and returns the initial state *)

val ga_print: ga_state -> unit
(** [ga_print s] prints the state of the current generation *)

val ga_steps: ga_state -> ga_state
(** [ga_steps s] evolves n generations and return the resulting state *)

val ga_step: ga_state -> ga_state
(** [ga_step s] evolves a generation and returns the new state *)

val log: ('a -> 'b, out_channel, unit, unit, unit, unit) format6 -> 'a -> 'b
(** printf-like function for loggin results *)