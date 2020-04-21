open Lambda_gp;;
open Lambda;;
open L;;
open Genetic;;
open Bool;;


let pair a b = App(App(Abs("x",Abs("y",Abs("z",App(App(Var "z",Var "x"),Var "y")))), a), b);;

let selector_fitness t =
  let a = Bool.of_bool @@ Random.bool () in 
  let b = Bool.of_bool @@ Random.bool () in 
  let p = pair a b in
  match reduce_fix_timeout @@ App(App(t, p), Bool.ltrue), reduce_fix_timeout @@ App(App(t, p), Bool.lfalse) with
  | a1, b1 when a1 = a && b1 = b -> 1.0
  | a1, b1 when a1 = a -> 0.5
  | a1, b1 when b1 = b -> 0.5
  | _, _ -> 0.0
;;

let s = ga_init {
  pop_size= 32;
  term_len= 12;
  var_n= 6;
  gen_n=500000;
  fitness_target= 1.0;
  test_best_f= (fun t -> (selector_fitness t) = 1.0);
  fitness_f= (fun t -> Helpers.cumulative_apply 128 (fun () -> selector_fitness t));
  valid_f= (fun t -> true);
} in ga_print s; ga_steps s; 