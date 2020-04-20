open Lambda_gp;;
open Lambda;;
open L;;
open Genetic;;
open Bool;;

let id_fitness t =
  let i = if Random.bool () then ltrue else lfalse in
  let res = reduce 4 @@ App(t, i) in
  if res = i then 1.0 else 0.0
;;

let s = ga_init {
  pop_size= 6;
  term_len= 3;
  var_n= 2;
  gen_n=1000;
  fitness_target= 1.0;
  test_best_f= (fun t -> (id_fitness t ) = 1.0);
  fitness_f= (fun t -> Helpers.cumulative_apply 2 (fun () -> id_fitness t));
  valid_f= (fun t -> true);
} in ga_print s; ga_steps s; 