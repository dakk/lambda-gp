open Lambda_gp;;
open Lambda;;
open Genetic;;

let s = ga_init {
  pop_size= 64;
  term_len= 19;
  var_n= 6;
  gen_n=50000;
  fitness_target= 1.0;
  test_best_f= (fun t -> 
      let r = 10 + Random.int 15 in
      try (
        let mul2 = reduce 9 @@ App(t, Church.of_int r) in 
        let res = Church.to_int mul2 in
        if res = r * 2 then true else false )
      with | _ -> false
  );
  fitness_f= (fun t -> 
    let rf () =
      let r = 2 + Random.int 9 in
      let mul2 = reduce 9 @@ App(t, Church.of_int r) in 
      match mul2 with 
      | t' when Church.is_church t' -> 
        let r' = Church.to_int mul2 in
        (* Printf.printf "%d %d %d\n%!" r r' (r*2); *)
        (match r' with
        | n when r * 2 = n -> 1.0 
        | n when n < r -> 0.0
        | n when n > r * 3 -> 0.0
        | n -> 
          let r' = if n > r * 2 then n - r else n in
          let diff = r * 2 - r' in
          let span = r * 2 - r in
          let rr = float (span - diff) /. (float (span) +. 0.01) in
          rr)
      | _ -> 0.0
    in rf () *. rf () *. rf ()

  );
  valid_f= (fun t -> 
    Church.is_church @@ reduce 8 (App(t, Church.of_int 1))
  );
} in ga_print s; ga_steps s;
