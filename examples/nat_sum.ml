open Lambda_gp;;
open Lambda;;
open Genetic;;

let s = ga_init {
  pop_size= 16;
  term_len= 64;
  var_n= 6;
  gen_n=500000;
  fitness_target= 1.0;
  test_best_f= (fun t -> 
    let r1 = 1 + Random.int 4 in
    let r2 = 1 + Random.int 4 in
      try (
        let sum = reduce_fix @@ App(App(t, Church.of_int r1), Church.of_int r2) in 
        let res = Church.to_int sum in
        if res = r1 + r2 then true else false )
      with | _ -> false
  );
  fitness_f= (fun t -> 
    let rf () =
      let r1 = 1 + Random.int 4 in
      let r2 = 1 + Random.int 4 in
      let sum = reduce_fix @@ App(App(t, Church.of_int r1), Church.of_int r2) in 
      match sum with 
      | t' when Church.is_church t' -> 
        let r' = Church.to_int sum in
        (* Printf.printf "%d %d %d\n%!" r r' (r*2); *)
        (match r' with
        | n when r1 + r2 = n -> 1.0 
        | n when n > r1 + r2 -> 0.7
        | n when n < r1 + r2 && n > r1 && n > r2 -> 0.5
        | n -> 0.01)
      | _ -> 0.0
    in rf () *. rf () *. rf () *. rf () *. rf ()

  );
  valid_f= (fun t -> 
    Church.is_church @@ reduce_fix (App(App(t, Church.of_int 1), Church.of_int 1))
  );
} in ga_print s; ga_steps s;
