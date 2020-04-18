open Lambda_gp;;
open Lambda;;
open Genetic;;
open Bool;;

let not_fitness t b =
  let res = reduce 4 @@ App(t, if b then ltrue else lfalse) in
  (* Printf.printf "%b => %b but %s\n" b (not b) (Lambda.to_string res); *)
  match b, res with
    false, ltrue -> 1.0
  | true, lfalse -> 1.0
  | _, _ -> 0.0
;;

let s = ga_init {
  pop_size= 32;
  term_len= 2;
  var_n= 2;
  gen_n=1000;
  fitness_target= 1.0;
  test_best_f= (fun t -> (not_fitness t @@ Random.bool ()) = 1.0);
  fitness_f= (fun t -> (not_fitness t true) *. (not_fitness t false));
  valid_f= (fun t -> Bool.is_bool @@ reduce 4 (App(t, Bool.lfalse)));
} in ga_print s; ga_steps s; 