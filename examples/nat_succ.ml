open Lambda_gp;;
open Lambda;;
open L;;
open Genetic;;

let s = ga_init {
  pop_size= 256;
  term_len= 17;
  var_n= 6;
  gen_n=5000000;
  fitness_target= 1.0;
  test_best_f= (fun t -> 
      let r = 1 + Random.int 15 in
      try (
        let succ = reduce 7 @@ App(t, Church.of_int r) in 
        (Church.to_int succ) = r + 1)
      with | _ -> false
  );
  fitness_f= (fun t -> Helpers.cumulative_apply 5 (fun () ->
	let r = 1 + Random.int 15 in
	let succ = reduce 7 @@ App(t, Church.of_int r) in 
	match succ with 
	| t' when Church.is_church t' && Church.to_int(succ) = r+1 -> 1.0
	(* | t' when Church.is_church t' && Church.to_int(succ) = r-1 -> 0.2
	| t' when Church.is_church t' && Church.to_int(succ) = r+2 -> 0.2
	| t' when Church.is_church t' && Church.to_int(succ) = r-2 -> 0.2
	| t' when Church.is_church t' && Church.to_int(succ) = r -> 0.12 *)
	| t' when Church.is_church t' -> 0.1
	| _ -> 0.0)
  );
  valid_f= (fun t -> Church.is_church @@ reduce 7 (App(t, Church.of_int 1)));
} in ga_print s; ga_steps s;
