open Lambda_gp;;
open Lambda;;
open Genetic;;
open Bool;;

let or_fitness t b b' =
  let res = reduce 4 @@ App(App(t, if b then ltrue else lfalse), if b' then ltrue else lfalse) in
  match b, b', res with
      false, true, r when r=ltrue -> 1.0
    | true, false, r when r=ltrue -> 1.0
    | false, false, r when r=lfalse -> 1.0
    | true, true, r when r=ltrue -> 1.0
    | _, _, _ -> 0.0
;;

let s = ga_init {
  pop_size= 32;
  term_len= 6;
  var_n= 3;
  gen_n=1000;
  fitness_target= 1.0;
  test_best_f= (fun t -> (or_fitness t (Random.bool ()) (Random.bool ())) = 1.0);
  fitness_f= (fun t -> 
    (or_fitness t true true) *. 
    (or_fitness t true false) *.
    (or_fitness t false false) *.
    (or_fitness t false true)
  );
  valid_f= (fun t -> Bool.is_bool @@ reduce 4 (App(App(t, lfalse), lfalse)));
} in ga_print s; ga_steps s;