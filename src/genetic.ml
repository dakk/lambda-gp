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


(* Replace an inner position of t with it *)
let rec replace_tree t position it = 
  if position = 0 then it else match t with
  | Lambda.Var x -> Lambda.Var x
  | Lambda.Abs(s, at) -> Lambda.Abs(s, replace_tree at (position - 1) it)
  | Lambda.App(at, at') -> 
    (* this is wrong, it replaces a tree in two branches *)
    Lambda.App(replace_tree at (position - 1) it, replace_tree at' (position - 1) it)
;;

let mutate s t = replace_tree t 
  (Random.int @@ Lambda.len t) 
  (Rand_term.generate_l (s.settings.term_len / 2) s.settings.var_n)
;;


(** A crossover is an exchange of different term tree; this function does k crossovers *)
let rec crossover_k k (t,t') = 
  let rec get_inner_term t l = if l = 0 then t else match t with
  | Lambda.Var x -> t
  | Lambda.Abs(s, at) -> get_inner_term at (l-1)
  | Lambda.App(at, at') -> 
    if Random.int 2 == 0 then
      get_inner_term at (l-1)
    else
      get_inner_term at' (l-1)
  in
  match k with 
  | 0 -> (t, t')
  | k ->
    (** find a random subtree of t *)
    let pos = Random.int @@ Lambda.len t in
    let it = get_inner_term t pos in
    (* printf "CRO1 %d %s\n" pos @@ Lambda.to_string it; *)
    (** find a random subtree of t' *)
    let pos' = Random.int @@ Lambda.len t' in 
    let it' = get_inner_term t' pos in
    (* printf "CRO2 %d %s\n" pos' @@ Lambda.to_string it';
    printf "CROR1 %s\n" @@ Lambda.to_string @@ replace_tree t pos it';
    printf "CROR2 %s\n" @@ Lambda.to_string @@ replace_tree t' pos' it; *)
    (** exchange them*)
    crossover_k (k-1) (replace_tree t pos it', replace_tree t' pos' it)
;;

let crossover (t,t') = crossover_k 1 (t,t');;

let fitness_stat_of_pop s p =
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
  let pop_fit = fitness_stat_of_pop s pop in
  {
    settings= s;
    population= pop;
    avg_fitness= fst pop_fit;
    best_fitness= snd pop_fit;
    generation= 0;
  }
;;

let ga_print s = 
  (* List.iter (fun i -> log "[%d] => [%f] %s" s.generation (snd i) (Lambda.to_string @@ fst i)) s.population; *)
  log "[%d] => %f best, %f avg" s.generation s.best_fitness s.avg_fitness;
  (* printf "\n" *)
  ()
;;

let ga_step s = 
  let rec fix_size (tl: Lambda.term list) = match List.length tl with
  | l when l = s.settings.pop_size -> tl
  | l when l > s.settings.pop_size -> fix_size @@ List.tl tl
  | l when l < s.settings.pop_size -> fix_size @@ (List.nth tl (Random.int l))::tl
  in
  let rec apply_cross (tl: Lambda.term list) = match tl with
    [] -> []
  | t::[] -> [t] 
  | t::t'::tl' ->
    let tcross = crossover (t,t') in
    (fst tcross)::(snd tcross)::(apply_cross tl')
  in
  let genn = s.generation + 1 in
  let best_parents = select_best_parents s.population (s.settings.pop_size / 2) in
  (* log "%d" (List.length best_parents); *)
  let best_terms = terms_of_pop best_parents in
  (* List.iter (fun t -> printf "BES %s\n%!" @@ Lambda.to_string t) best_terms; *)

  (* Crossover of parents *)
  let cross_pop = apply_cross best_terms in
  (* List.iter (fun t -> printf "CRO %s\n%!" @@ Lambda.to_string t) cross_pop; *)

  (* Mutation of crossovers? *)
  let mut_pop = List.map (fun t -> mutate s t) cross_pop in
  (* List.iter (fun t -> printf "MUT %s\n%!" @@ Lambda.to_string t) mut_pop; *)

  (* List.iter (fun tl -> List.iter (fun x -> printf "%s\n%!" x) @@ Lambda.fv_l tl) mut_pop;
  List.iter (fun tl -> List.iter (fun x -> printf "%s\n%!" x) @@ Lambda.fv_l tl) best_terms; *)
  (* Assert that the population has the right size *)
  let pop = fix_size (best_terms @ mut_pop) in

  (* Create the new population with parents and mutations *)
  let npop = pop_of_terms s @@ pop in  

  let pop_fit = fitness_stat_of_pop s npop in
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
  let (t, f) = select_best s' in
  if f >= s'.settings.fitness_target then (
    log "found a best at generation %d" s'.generation;
    log "best is %s with a fitness of %f" (Lambda.to_string t) f;
    s'
  ) else
    ga_steps s'
;;
