open Printf;;
open Helpers;;
open Lambda;;

type fitness_f = L.term -> float;;
type valid_f = L.term -> bool;;
type test_best_f = L.term -> bool;;

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


type actual_best = {
	score: int;
	len: int;
	term: L.term;
};;

type ga_state = {
  settings: ga_settings;
  population: (L.term * float) list;
  avg_fitness: float;
  avg_term_len: int;
  best_fitness: float;
  actual_best: actual_best option;
  generation: int;
};;

let log s fmt = printf ("<λ-gp> " ^^ s ^^ "\n%!") fmt;;


(* Replace an inner position of t with it *)
let replace_tree t pos it =
  let rec replace_tree' t pos it = 
    if pos = 0 then (0, it) else match t with
    | L.Var x -> (pos-1, L.Var x)
    | L.Abs(s, at) -> 
      let inp = replace_tree' at (pos - 1) it in
      if fst inp = 0 then (0, L.Abs(s, snd inp)) else (fst inp, L.Abs(s, at))
    | L.App(at, at') -> 
      let inpa = replace_tree' at (pos - 1) it in
      if fst inpa = 0 then (0, L.App(snd inpa, at')) else 
        let inpb = replace_tree' at' (fst inpa) it in
        if fst inpb = 0 then (0, L.App(at, snd inpb))
        else (fst inpb, L.App(at, at'))
  in snd @@ replace_tree' t pos it
;;


let get_inner_term t pos =
  let rec get_inner_term' t pos = 
    if pos = 0 then (0, t) else match t with
    | L.Var x -> (pos-1, L.Var x)
    | L.Abs(s, at) -> 
      let inp = get_inner_term' at (pos - 1) in
      if fst inp = 0 then (0, L.Abs(s, snd inp)) else (fst inp, L.Abs(s, at))
    | L.App(at, at') -> 
      let inpa = get_inner_term' at (pos - 1) in
      if fst inpa = 0 then (0, L.App(snd inpa, at')) else 
        let inpb = get_inner_term' at' (fst inpa) in
        if fst inpb = 0 then (0, L.App(at, snd inpb))
        else (fst inpb, L.App(at, at'))
  in snd @@ get_inner_term' t pos
;;


let mutate_random s t =
  replace_tree t 
  (1 + (Random.int @@ L.len t)) 
  (Rand_term.generate_l2 (1 + (Random.int s.settings.term_len)) (1 + (Random.int @@ s.settings.var_n - 1)) [])
;;

let mutate_redex s t =
  L.reduce (Random.int s.settings.var_n) t
;;

let u = L.Abs("x",Abs("y",App(Var "y",App(App(Var "x",Var "x"),Var "y"))));;
let theta = L.App(u,u);;

let mutate_fp s t = L.App(theta, t);;

let mutate_drop s t =
  replace_tree t 
  (Random.int @@ L.len t) 
  (Var (L.get_var @@ Random.int @@ s.settings.var_n - 1))
;;

let rec mutate_harvest s t = if L.len t <= s.settings.term_len * 4 then t else 
  mutate_harvest s @@ mutate_drop s t
;;

let mutate s t = 
  replace_tree t (Random.int @@ L.len t) 
    (Rand_term.generate_l2 
      (1 + (Random.int @@ s.settings.term_len / 2))
      (1 + (Random.int @@ s.settings.var_n - 1))
      []
    )
;;


let crossover (t,t') = 
  (* find a random subtree of t *)
  (* printf "DBG CR 1 => %s\n" @@ L.to_string t;
  printf "DBG CR 2 =>  %s\n" @@ L.to_string t'; *)
  let pos = Random.int @@ L.len t in
  let it = get_inner_term t pos in
  (* printf "DBG CR Piece1 =>  %d %s\n" pos @@ L.to_string it; *)
  (* find a random subtree of t' *)
  let pos' = Random.int @@ L.len t' in 
  let it' = get_inner_term t' pos in
  (* printf "DBG CR Piece2 => %d %s\n" pos' @@ L.to_string it';
  printf "DBG CR Result1 => %s\n" @@ L.to_string @@ replace_tree t pos it';
  printf "DBG CR Result2 => %s\n\n" @@ L.to_string @@ replace_tree t' pos' it; *)
  (* exchange them*)
  (replace_tree t pos it', replace_tree t' pos' it)
;;


let fitness_stat_of_pop p =
  let rec pop_fitness' p c = match p, c with
  | [], c -> c
  | (t, f)::p', (l, a, b) -> 
    if f > b  then 
      pop_fitness' p' (l + L.len t, a +. f,f) else pop_fitness' p' (l + L.len t, a +. f,b)
  in 
  match pop_fitness' p (0, 0., 0.) with
  | l, a, b -> (l / List.length p, a /. float(List.length p), b)
;;

let select_best s =
  let rec sl p = match p with
  | [] -> failwith "No best found"
  | (t, f)::p' -> if f = s.best_fitness then (t |> L.eta_conversion,f) else sl p'
  in sl s.population
;;

let sort_population p = List.sort (fun x y -> int_of_float @@ (snd y -. snd x) *. 1000000.) p;;

let rec sublist b e l = match l with
    [] -> []
  | h :: t -> 
    let tail = if e=0 then [] else sublist (b-1) (e-1) t in
    if b>0 then tail else h :: tail
;;

let selection p n =
  sublist 0 n p
   (* |> sort_population) n *)
;;


let selection2 p n =
  let rec selection' p n = 
    let rec select p r = match p with 
    | [] -> failwith "none"
    | (nf, x)::p' when nf > r -> x
    | a::p' -> try select p' r with | _ -> snd a
    in
    let r = Random.float 1.0 in
    if n = 0 then [] else (select p r)::selection' p (n-1)
  in
  let p = List.rev p in
  let fsum = List.fold_left (fun y x -> (snd x)+.y) 0.0 p in
  let p = List.map (fun x -> (snd x /. fsum, x)) p in
  let pacc = List.fold_left (fun y x -> (fst x +. (if List.length y = 0 then 0.0 else fst @@ List.hd y), snd x)::y) [] p in
  selection' pacc n
;;

let rec terms_of_pop p = match p with
| [] -> []
| (t,_)::p' -> t::(terms_of_pop p') 
;;

let rec pop_of_terms s l = match l with
| [] -> []
| t::l' -> (t, s.settings.fitness_f t)::(pop_of_terms s l')
;;

(* let rec fix_size s (tl: L.term list) = match List.length tl with
| l when l = s.settings.pop_size -> tl
| l when l > s.settings.pop_size -> fix_size s @@ List.tl tl
| l when l < s.settings.pop_size -> fix_size s @@ (List.nth tl (Random.int l))::tl
;; *)


let rec check_env_unbound t env = match t with 
  L.Var x -> if not (List.mem x env) then failwith ("Unbound variable " ^ x) else true
| Abs(x, f) -> check_env_unbound f (x::env)
| App(f, p) -> check_env_unbound p env
;;

let check_term_bound t = try check_env_unbound t [] with | _ -> false;;

let ga_init s =   
  let rec gen_init_pop l = match l with
  | 0 -> []
  | _ -> 
    let nt = Rand_term.generate_l2 s.term_len s.var_n [] in 
    (* let nt = Rand_term.generate s.term_len s.var_n in  *)
    (* List.iter (fun x -> printf "%s\n%!" x) @@ L.fv_l nt; *)
    (* L.len nt < s.term_len || not (check_term_bound nt)  *)
    if not (s.valid_f nt) then gen_init_pop l 
    else (nt, s.fitness_f nt)::(gen_init_pop (l-1))
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
    actual_best= None;
  }
;;

let print_actual_best s = match s.actual_best with 
  | None -> ()
  | Some b -> 
    log "[%d] => [actual_best] [len: %d, rate: %d%%] => %s" s.generation b.len b.score (L.to_string b.term)
;;

let ga_print s = 
  print_actual_best s;
  (* List.iter (fun i -> 
    log "[%d] => [%f] %s (%d)" s.generation (snd i) (L.to_string @@ fst i) (L.len @@ fst i)
  ) s.population; *)
  log "[%d] => %f best, %f avg, %d avg term len" s.generation s.best_fitness s.avg_fitness s.avg_term_len;
  (* printf "\n"; *)
  ()
;;

(* let return_bounded t' t  = if check_term_bound t then t else t';; *)

let ga_step s = 
  (* let rec cross_valid t t' = 
    let tcross = crossover (t,t') in 
    if not (check_term_bound @@ fst tcross) || not (check_term_bound @@ snd tcross) 
    then cross_valid t t' else tcross
  in *)
  let rec apply_cross (tl: L.term list) = match tl with
		[] -> []
	| t::[] -> let tcross = crossover (t, t) in (fst tcross)::(snd tcross)::[]
	| t::t'::tl' -> let tcross = crossover (t, t') in (fst tcross)::(snd tcross)::(apply_cross tl')
  in
  let genn = s.generation + 1 in

  (* select best_parent using a probability function *)
  let selected_pop = selection s.population (s.settings.pop_size / 2) in
  let selected_terms = terms_of_pop @@ selected_pop in

  (* Crossover of parents *)
  let cross_pop = ((try [fst @@ select_best s] with | _ -> []) @ selected_terms) |> Helpers.shuffle |> apply_cross in

  (* Mutations *)
  let mut_cross_pop = cross_pop
    |> List.map (fun t -> if (Random.int 100 < 20) then mutate s t else t) 
    (* |> List.map (fun t -> if (Random.int 100 < 50) then mutate_drop s t else t)  *)
    (* |> List.map (fun t -> if (Random.int 100 < 10) then mutate_redex s t else t) *)
    |> List.map (fun t -> if (Random.int 100 < 95) then mutate_harvest s t else t)
    |> List.map (fun t -> if (Random.int 100 < 20) then mutate_random s t else t) 
    |> List.map (fun t -> if (Random.int 100 < 30) then L.eta_conversion t else t)
    |> List.map (fun t -> if (Random.int 100 < 10) then mutate_fp s t else t)
    |> List.map (fun t -> if (Random.int 100 > 10) then t else 
      L.alfa_conversion (Rand_term.rand_var s.settings.var_n) (Rand_term.rand_var s.settings.var_n) t)
    (* |> List.map (fun t -> if check_term_bound t then t else fix_term_unbound t) *)
  in

  let npop = selected_terms @ mut_cross_pop in
  let npop = (try [select_best s] with | _-> []) @
  	( (pop_of_terms s npop) |> sort_population |> sublist 0 s.settings.pop_size ) 
	  @ pop_of_terms s [
      Rand_term.generate (s.settings.term_len * 2) (s.settings.var_n + 1);
      Rand_term.generate (s.settings.term_len / 2) (s.settings.var_n + 1)
    ]
  in match fitness_stat_of_pop npop with
  | tl, af, bf -> { s with 
    avg_term_len= tl;
    avg_fitness= af;
    best_fitness= bf;
    generation= genn;
    population= npop |> sort_population;
  }
;;

let rec ga_steps s = 
  let rec remove_best p t = match p with 
    | [] -> []
    | (t',f)::tl when t=t' -> remove_best tl t
    | (t',f)::tl -> (t',f)::(remove_best tl t)
  in
  let best_test t s' =
    let rec rt n succ = if n = 0 then succ else match s'.settings.test_best_f t with
      true -> rt (n-1) (succ+1)
    | false -> rt (n-1) succ
    in 
    let best_times = 500 in
    let success = rt best_times 0 in
    let perc = int_of_float((float success) *. 100.0 /. (float best_times)) in
    perc
  in 
  if s.generation = s.settings.gen_n then (print_actual_best s; s) else (
    let s' = ga_step s in ga_print s';
    let (t, f) = select_best s' in
    if f < s'.settings.fitness_target then ga_steps s' else (
      let perc = best_test t s' in 
		  let nactbest = Some ({ len= L.len t; score= perc; term= t; }) in
      match s'.actual_best with 
      | None -> ga_steps { s' with 
        population= remove_best s'.population t;
      	best_fitness= 0.0;
        actual_best= nactbest; }
      | Some b when b.term <> t && ((b.len > (L.len t) && b.score <= perc) || (b.score < perc)) -> ga_steps { 
        s' with 
        population= remove_best s'.population t;
        best_fitness= 0.0;
        actual_best= nactbest; }
      | _ -> ga_steps { s' with population=remove_best s'.population t; best_fitness=0.0}
    )
  )
;;
