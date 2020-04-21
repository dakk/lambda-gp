open Lambda_gp;;
open Lambda;;
open L;;
open Genetic;;

let rec fact x = if x <= 1 then 1 else x * fact (x - 1);;

let s = ga_init {
  pop_size= 64;
  term_len= 64;
  var_n= 6;
  gen_n=500000;
  fitness_target= 1.0;
  test_best_f= (fun t -> 
    let r = 1 + Random.int 9 in
      try (
        let fac = reduce_fix_timeout ~n:1024 @@ App(t, Church.of_int r) in 
        let res = Church.to_int fac in
        if res = fact r then true else false )
      with | _ -> false
  );
  fitness_f= (fun t -> Helpers.cumulative_apply 5 (fun () ->
	let r = 1 + Random.int 9 in
	let fac = reduce_fix_timeout ~n:1024 @@ App(t, Church.of_int r) in 
	match fac with 
	| t' when Church.is_church t' -> 
		let r' = Church.to_int fac in
		(* Printf.printf "%d %d %d\n%!" r r' (r*2); *)
		(match r' with
		| n when fact r = n -> 1.0 
		| n when n > fact r -> 0.7
		| n when n < fact r && n > r -> 0.2
		| n -> 0.01)
	| _ -> 0.0)
  );
  valid_f= (fun t -> 
    Church.is_church @@ reduce_fix_timeout (App(t, Church.of_int 1))
  );
} in ga_print s; ga_steps s;
