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


val ga_init: ga_settings -> ga_state
val ga_print: ga_state -> unit
val ga_steps: ga_state -> ga_state
val ga_step: ga_state -> ga_state
val log: ('a -> 'b, out_channel, unit, unit, unit, unit) format6 -> 'a -> 'b