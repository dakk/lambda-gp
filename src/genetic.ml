open Printf;;

type fitness_f = Lambda.term -> float;;
type valid_f = Lambda.term -> bool;;
type test_best_f = Lambda.term -> bool;;

type ga_settings = {
  test_best_f: test_best_f;
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
  avg_term_len: int;
  best_fitness: float;
  generation: int;
};;

let log s fmt = printf ("<λ-gp> " ^^ s ^^ "\n%!") fmt;;


(* Replace an inner position of t with it *)
let replace_tree t pos it =
  let rec replace_tree' t pos it = 
    if pos = 0 then (0, it) else match t with
    | Lambda.Var x -> (pos-1, Lambda.Var x)
    | Lambda.Abs(s, at) -> 
      let inp = replace_tree' at (pos - 1) it in
      if fst inp = 0 then (0, Lambda.Abs(s, snd inp)) else (fst inp, Lambda.Abs(s, at))
    | Lambda.App(at, at') -> 
      let inpa = replace_tree' at (pos - 1) it in
      if fst inpa = 0 then (0, Lambda.App(snd inpa, at')) else 
        let inpb = replace_tree' at' (fst inpa) it in
        if fst inpb = 0 then (0, Lambda.App(at, snd inpb))
        else (fst inpb, Lambda.App(at, at'))
  in snd @@ replace_tree' t pos it
;;


let get_inner_term t pos =
  let rec get_inner_term' t pos = 
    if pos = 0 then (0, t) else match t with
    | Lambda.Var x -> (pos-1, Lambda.Var x)
    | Lambda.Abs(s, at) -> 
      let inp = get_inner_term' at (pos - 1) in
      if fst inp = 0 then (0, Lambda.Abs(s, snd inp)) else (fst inp, Lambda.Abs(s, at))
    | Lambda.App(at, at') -> 
      let inpa = get_inner_term' at (pos - 1) in
      if fst inpa = 0 then (0, Lambda.App(snd inpa, at')) else 
        let inpb = get_inner_term' at' (fst inpa) in
        if fst inpb = 0 then (0, Lambda.App(at, snd inpb))
        else (fst inpb, Lambda.App(at, at'))
  in snd @@ get_inner_term' t pos
;;


let mutate_random s t =
  replace_tree t 
  (1 + (Random.int @@ Lambda.len t)) 
  (Rand_term.generate_l (1 + (Random.int s.settings.term_len)) @@ 1 + (Random.int @@ s.settings.var_n - 1))
;;

let mutate_redex s t =
  Lambda.reduce (Random.int s.settings.var_n) t
;;

let mutate_drop s t =
  replace_tree t 
  (Random.int @@ Lambda.len t) 
  (Var (Lambda.get_var @@ Random.int @@ s.settings.var_n - 1))
;;

let rec mutate_harvest s t = if Lambda.len t <= s.settings.term_len * 4 then t else 
  mutate_harvest s @@ mutate_drop s t
;;

let mutate s t = 
  replace_tree t (Random.int @@ Lambda.len t) 
    (Rand_term.generate_l 
      (1 + (Random.int @@ s.settings.term_len / 2))
      (1 + (Random.int @@ s.settings.var_n - 1))
    )
;;


let crossover (t,t') = 
  (* find a random subtree of t *)
  (* printf "DBG CR 1 => %s\n" @@ Lambda.to_string t;
  printf "DBG CR 2 =>  %s\n" @@ Lambda.to_string t'; *)
  let pos = Random.int @@ Lambda.len t in
  let it = get_inner_term t pos in
  (* printf "DBG CR Piece1 =>  %d %s\n" pos @@ Lambda.to_string it; *)
  (* find a random subtree of t' *)
  let pos' = Random.int @@ Lambda.len t' in 
  let it' = get_inner_term t' pos in
  (* printf "DBG CR Piece2 => %d %s\n" pos' @@ Lambda.to_string it';
  printf "DBG CR Result1 => %s\n" @@ Lambda.to_string @@ replace_tree t pos it';
  printf "DBG CR Result2 => %s\n\n" @@ Lambda.to_string @@ replace_tree t' pos' it; *)
  (* exchange them*)
  (replace_tree t pos it', replace_tree t' pos' it)
;;


let fitness_stat_of_pop p =
  let rec pop_fitness' p c = match p, c with
  | [], c -> c
  | (t, f)::p', (l, a, b) -> 
    if f > b  then 
      pop_fitness' p' (l + Lambda.len t, a +. f,f) else pop_fitness' p' (l + Lambda.len t, a +. f,b)
  in 
  match pop_fitness' p (0, 0., 0.) with
  | l, a, b -> (l / List.length p, a /. float(List.length p), b)
;;

let select_best s =
  let rec sl p = match p with
  | [] -> failwith "No best found"
  | (t, f)::p' -> if f = s.best_fitness then (t,f) else sl p'
  in sl s.population
;;

let sort_population p = List.sort (fun x y -> int_of_float @@ (snd y -. snd x) *. 1000000.) p;;

let rec sublist b e l = match l with
    [] -> failwith "sublist"
  | h :: t -> 
    let tail = if e=0 then [] else sublist (b-1) (e-1) t in
    if b>0 then tail else h :: tail
;;

let select_best_parents p n =
  sublist 0 n p
   (* |> sort_population) n *)
;;

let rec terms_of_pop p = match p with
| [] -> []
| (t,_)::p' -> t::(terms_of_pop p') 
;;

let rec pop_of_terms s l = match l with
| [] -> []
| t::l' -> (t, s.settings.fitness_f t)::(pop_of_terms s l')
;;

(* let rec fix_size s (tl: Lambda.term list) = match List.length tl with
| l when l = s.settings.pop_size -> tl
| l when l > s.settings.pop_size -> fix_size s @@ List.tl tl
| l when l < s.settings.pop_size -> fix_size s @@ (List.nth tl (Random.int l))::tl
;; *)

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
  match fitness_stat_of_pop pop with
  | tl, af, bf -> { 
    settings= s;
    population= pop |> sort_population;
    avg_fitness= af;
    avg_term_len= tl;
    best_fitness= bf;
    generation= 0;
  }
;;

let ga_print s = 
  (* List.iter (fun i -> 
    log "[%d] => [%f] %s (%d)" s.generation (snd i) (Lambda.to_string @@ fst i) (Lambda.len @@ fst i)
  ) s.population; *)
  log "[%d] => %f best, %f avg, %d avg term len" s.generation s.best_fitness s.avg_fitness s.avg_term_len;
  (* printf "\n"; *)
  ()
;;


let ga_step s = 
  let rec apply_cross (tl: Lambda.term list) = match tl with
    [] -> []
  | t::[] -> 
    let tcross = crossover (t,t) in
    (fst tcross)::(snd tcross)::[]
  | t::t'::tl' ->
    let tcross = crossover (t,t') in
    (fst tcross)::(snd tcross)::(apply_cross tl')
  in
  let genn = s.generation + 1 in
  let best_parents = select_best_parents s.population (s.settings.pop_size / 2) in
  (* let best_terms = terms_of_pop best_parents in *)
  let best_terms = terms_of_pop @@ best_parents in

  (* Crossover of parents *)
  let cross_pop = ([fst @@ select_best s] @ best_terms) |> Helpers.shuffle |> apply_cross in

  (* Mutations *)
  let mut_cross_pop = cross_pop
    |> List.map (fun t -> if (Random.int 100 < 20) then mutate s t else t) 
    (* |> List.map (fun t -> if (Random.int 100 < 50) then mutate_drop s t else t)  *)
    |> List.map (fun t -> if (Random.int 100 < 10) then mutate_redex s t else t)
    |> List.map (fun t -> if (Random.int 100 < 90) then mutate_harvest s t else t)
    |> List.map (fun t -> if (Random.int 100 < 20) then mutate_random s t else t)
    |> List.map (fun t -> if (Random.int 100 < 30) then Lambda.eta_conversion t else t)
    |> List.map (fun t -> if (Random.int 100 < 10) then 
      Lambda.alfa_conversion (Rand_term.rand_var s.settings.var_n) (Rand_term.rand_var s.settings.var_n) t
	else t) in

  let best_pop = best_terms in
  let npop = best_pop @ mut_cross_pop in
  let npop = [select_best s] @
  	( (pop_of_terms s npop) |> sort_population |> sublist 0 s.settings.pop_size ) 
	  @ pop_of_terms s [Rand_term.generate s.settings.term_len s.settings.var_n]
  in  

  match fitness_stat_of_pop npop with
  | tl, af, bf -> { s with 
    avg_term_len= tl;
    avg_fitness= af;
    best_fitness= bf;
    generation= genn;
    population= npop;
  };;

let rec ga_steps s = 
  let best_test t s' =
    let best_times = 100 in
    log "running test_best %d times" best_times;
    let rec rt n succ = if n = 0 then succ else match s'.settings.test_best_f t with
      true -> rt (n-1) (succ+1)
    | false -> rt (n-1) succ
    in 
    let success = rt best_times 0 in
    log "test_best(n: %d) => %d success (%d%%)" best_times success @@ int_of_float((float success) /. 100.0 *. (float best_times))
  in 
  match s.generation with 
  | n when n = s.settings.gen_n -> 
    let (t, f) = select_best s in
    log "best is %s with a fitness of %f" (Lambda.to_string t) f;
    best_test t s;
    s
  | _ -> 
    let s' = ga_step s in
    ga_print s';
    let (t, f) = select_best s' in
    if f >= s'.settings.fitness_target then (
      log "found a best at generation %d" s'.generation;
      log "best is %s with a fitness of %f" (Lambda.to_string t) f;
      best_test t s';
      s'
    ) else ga_steps s'
;;
