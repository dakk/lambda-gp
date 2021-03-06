open Lambda_gp;;
open Lambda;;
open L;;
open Genetic;;
open Bool;;

let fst = Abs("x",App(Var "x",Bool.ltrue));;
let snd = Abs("x",App(Var "x",Bool.lfalse));;

let pair_fitness t =
  let a = Bool.of_bool @@ Random.bool () in 
  let b = Bool.of_bool @@ Random.bool () in 
  let p = App(App(t, a), b) in
  match reduce_fix_timeout (App(fst, p)), reduce_fix_timeout (App(snd, p)) with
  | a1, b1 when a1 = a && b1 = b -> 1.0
  | a1, b1 when a1 = a -> 0.5
  | a1, b1 when b1 = b -> 0.5
  | _, _ -> 0.0
;;

let s = ga_init {
  pop_size= 512;
  term_len= 12;
  var_n= 3;
  gen_n=500000;
  fitness_target= 1.0;
  test_best_f= (fun t -> (pair_fitness t) = 1.0);
  fitness_f= (fun t -> Helpers.cumulative_apply 32 (fun () -> pair_fitness t));
  valid_f= (fun t -> true);
} in ga_print s; ga_steps s; 