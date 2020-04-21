open Lambda_gp;;
open Lambda;;
open L;;
open Genetic;;

let s = ga_init {
  pop_size= 256;
  term_len= 32;
  var_n= 6;
  gen_n=50000;
  fitness_target= 0.99;
  test_best_f= (fun t -> 
    let r = 1 + Random.int 16 in
    try (
      let mul3 = reduce_fix_timeout @@ App(t, Church.of_int r) in 
      let res = Church.to_int mul3 in
      if res = r * 3 then true else false )
    with | _ -> false
  );
  fitness_f= (fun t -> Helpers.cumulative_apply 10 (fun () ->
    let r = 1 + Random.int 12 in
    let mul3 = reduce_fix_timeout @@ App(t, Church.of_int r) in 
    match mul3 with 
    | t' when not (Church.is_church t') -> 0.0
    | t' ->
      let r' = Church.to_int mul3 in
      (* Printf.printf "%d %d %d\n%!" r r' (r*2); *)
      (match r' with
      | n when r * 3 = n -> 1.0 
      | n when n < r -> 0.01
      | n when n > r * 3 -> 0.01
      | n -> 
        let r' = if n > r * 3 then n - r else n in
        let diff = r * 3 - r' in
        let span = r * 3 - r in
        let rr = float (span - diff) /. (float (span) +. 0.01) in
        rr)
  ));
  valid_f= (fun t -> 
    Church.is_church @@ reduce 10 (App(t, Church.of_int 1))
  );
} in ga_print s; ga_steps s;
