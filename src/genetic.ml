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


let mutate t = t;;
let crossover_k n (t,t') = (t', t);;
let crossover (t,t') = crossover_k 1;;

let fitness_of_pop s p =
  let rec pop_fitness' p c = match p with
  | [] -> c
  | (t, f)::p' -> 
    if f > snd c  then pop_fitness' p' ((fst c) +. f,f) else pop_fitness' p' ((fst c) +. f,snd c)
  in 
  let res = pop_fitness' p (0., 0.) in
  (fst res /. float(List.length p), snd res)
;;

let select_best s =
  let rec sl p = match p with
  | (t, f)::p' -> if f = s.best_fitness then (t,f) else sl p'
  in sl s.population
;;

let select_best_parents p n =
  let rec sub l i = match (l,i) with
  | [], _ -> []
  | t::l', 0 -> []
  | t::l', n -> t::(sub l' (n-1))
  in
  let sorted = List.sort (fun x y -> int_of_float (snd y *. 10. -. snd x *. 10.0)) p in
  sub sorted n
;;

let rec terms_of_pop p = match p with
| [] -> []
| (t,f)::p' -> t::(terms_of_pop p') 
;;

let rec pop_of_terms s l = match l with
| [] -> []
| t::l' -> (t, s.settings.fitness_f t)::(pop_of_terms s l')
;;


let ga_init s =   
  let rec gen_init_pop l = match l with
  | 0 -> []
  | _ -> 
    let nt = Rand_term.generate s.term_len s.var_n in 
    (* List.iter (fun x -> printf "%s\n%!" x) @@ Lambda.fv_l nt; *)
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
    log "[%d] => [%f] %s" s.generation (snd i) (Lambda.to_string @@ fst i);
    print_pop p'
  in
  print_pop s.population;
  log "[%d] => %f best, %f avg" s.generation s.best_fitness s.avg_fitness;
  printf "\n"
;;

let ga_step s = 
  let genn = s.generation + 1 in
  let best_parents = select_best_parents s.population (s.settings.pop_size / 2) in
  (* log "%d" (List.length best_parents); *)
  let best_terms = terms_of_pop best_parents in

  (* Crossover of parents *)

  (* Mutation of crossovers? *)

  (* Create the new population with parents and mutations *)

  (* Assert that the population has the right size *)

  let npop = pop_of_terms s best_terms in
  let pop_fit = fitness_of_pop s npop in
  { s with 
    avg_fitness= fst pop_fit;
    best_fitness= snd pop_fit;
    generation= genn;
    population= npop;
  }
  ;;

let rec ga_steps s = match s.generation with 
| n when n = s.settings.gen_n -> 
  let (t, f) = select_best s in
  log "best is %s with a fitness of %f" (Lambda.to_string t) f;
  s
| _ -> 
  let s' = ga_step s in
  ga_print s';
  let (t, f) = select_best s in
  if f >= s.settings.fitness_target then (
    log "find a best at generation %d" s.generation;
    log "best is %s with a fitness of %f" (Lambda.to_string t) f;
    s
  ) else
    ga_steps s'
;;
