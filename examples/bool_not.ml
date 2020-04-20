open Lambda_gp;;
open Lambda;;
open L;;
open Genetic;;
open Bool;;

let not_fitness t b =
  let res = reduce_fix_timeout @@ App(t, if b then ltrue else lfalse) in
  match b, res with
    false, r when r=ltrue -> 1.0
  | true, r when r=lfalse -> 1.0
  | _, _ -> 0.0
;;

let s = ga_init {
  pop_size= 1024;
  term_len= 6;
  var_n= 4;
  gen_n=100000;
  fitness_target= 1.0;
  test_best_f= (fun t -> (not_fitness t @@ Random.bool ()) = 1.0);
  fitness_f= (fun t -> (not_fitness t true) *. (not_fitness t false));
  valid_f= (fun t -> Bool.is_bool @@ reduce_fix_timeout (App(t, lfalse)));
} in ga_print s; ga_steps s;;
