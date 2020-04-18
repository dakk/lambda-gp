open Lambda_gp;;
open Lambda;;
open Genetic;;
open Bool;;

let and_fitness t b b' =
  let res = reduce 4 @@ App(App(t, if b then ltrue else lfalse), if b' then ltrue else lfalse) in
  (* Printf.printf "%b => %b but %s\n" b (not b) (Lambda.to_string res); *)
  match b, b', res with
      false, false, ltrue -> 1.0
    | true, true, ltrue -> 1.0
    | false, true, lfalse -> 1.0
    | true, false, lfalse -> 1.0
    | _, _, _ -> 0.0
;;

let s = ga_init {
  pop_size= 32;
  term_len= 6;
  var_n= 2;
  gen_n=1000;
  fitness_target= 1.0;
  test_best_f= (fun t -> (and_fitness t (Random.bool ()) (Random.bool ())) = 1.0);
  fitness_f= (fun t -> 
    (and_fitness t true true) *. 
    (and_fitness t true false) *.
    (and_fitness t false false) *.
    (and_fitness t false true)
  );
  valid_f= (fun t -> Bool.is_bool @@ reduce 4 (App(App(t, lfalse), lfalse)));
} in ga_print s; ga_steps s;