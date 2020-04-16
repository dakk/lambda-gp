open Printf;;

type fitness_f = Lambda.term -> float;;
type valid_f = Lambda.term -> bool;;

type ga_settings = {
  fitness_f: fitness_f;
  valid_f: valid_f;
  fitness_target: float;
  pop_size: int;
  term_len: int;
  var_n: int;
  gen_n: int;
};;

type ga_state = {
  settings: ga_settings;
  population: (Lambda.term * float) list;
  avg_fitness: float;
  best_fitness: float;
  generation: int;
};;

let log s fmt = printf ("<λ-gp> " ^^ s ^^ "\n%!") fmt;;


let fitness_of_pop s p =
  let rec pop_fitness' p c = match p with
  | [] -> c
  | (t, f)::p' -> 
    if f > snd c  then pop_fitness' p' ((fst c) +. f,f) else pop_fitness' p' ((fst c) +. f,snd c)
  in 
  let res = pop_fitness' p (0., 0.) in
  (fst res /. float(List.length p), snd res)
;;

let ga_init s =   
  let rec gen_init_pop l = match l with
  | 0 -> []
  | _ -> 
    let nt = Rand_term.generate s.term_len s.var_n in 
    if Lambda.len nt < s.term_len || not (s.valid_f nt) then 
      gen_init_pop l 
    else 
      [(nt, s.fitness_f nt)] @ gen_init_pop (l-1) 
  in
  log "init [gens: %d] [pop_size: %d] [var: %d] [len: %d] [target: %f]" s.gen_n s.pop_size s.var_n s.term_len s.fitness_target;
  log "generating random population of %d terms [len: %d, var: %d]" s.pop_size s.term_len s.var_n;
  let pop = gen_init_pop s.pop_size in
  let pop_fit = fitness_of_pop s pop in
  {
    settings= s;
    population= pop;
    avg_fitness= fst pop_fit;
    best_fitness= snd pop_fit;
    generation= 0;
  }
;;

let ga_print s = 
  let rec print_pop p = match p with
  | [] -> ()
  | i::p' -> 
    log "[gen%d] => [%f] %s" s.generation (snd i) (Lambda.to_string @@ fst i);
    print_pop p'
  in
  print_pop s.population;
  log "[gen%d] => %f best, %f avg" s.generation s.best_fitness s.avg_fitness  
;;

let ga_step s = s;;
