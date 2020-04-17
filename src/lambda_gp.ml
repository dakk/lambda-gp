open Lambda;;
open Genetic;;
open Bool;;



(** Example 1: not function *)
(* let not_fitness t b =
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
} in ga_print s; ga_steps s; *)


(** Example 2: and function *)
(* let and_fitness t b b' =
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
} in ga_print s; ga_steps s; *)


(** Mul * 2 *)
(* let s = ga_init {
  pop_size= 256;
  term_len= 9;
  var_n= 6;
  gen_n=500;
  fitness_target= 1.0;
  test_best_f= (fun t -> 
      let r = 10 + Random.int 15 in
      try (
        let mul2 = reduce 6 @@ App(t, Church.of_int r) in 
        let res = Church.to_int mul2 in
        if res = r * 2 then true else false )
      with | _ -> false
  );
  fitness_f= (fun t -> 
    let rf () =
      let r = 2 + Random.int 9 in
      let mul2 = reduce_fix @@ App(t, Church.of_int r) in 
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
          (* Printf.printf " => %f\n%!" rr; *)
          rr)
      | _ -> 0.0
    in rf () *. rf () *. rf ()

  );
  valid_f= (fun t -> 
    (* log "%b" @@ has_redex @@ App(t, Bool.lfalse); 
    log "reducing %s" @@ to_string @@ App(t, Bool.lfalse);  *)
    Church.is_church @@ reduce_fix (App(t, Church.of_int 1))
  );
} in ga_print s; ga_steps s; *)


(** Sum *)
(* let s = ga_init {
  pop_size= 256;
  term_len= 48;
  var_n= 5;
  gen_n=5000;
  fitness_target= 1.0;
  test_best_f= (fun t -> 
      let r = 10 + Random.int 15 in
      try (
        let mul2 = reduce 6 @@ App(t, Church.of_int r) in 
        let res = Church.to_int mul2 in
        if res = r * 2 then true else false )
      with | _ -> false
  );
  fitness_f= (fun t -> 
    let rf () =
      let r1 = 2 + Random.int 9 in
      let r2 = 2 + Random.int 9 in
      let sum = reduce 6 @@ App(App(t, Church.of_int r1), Church.of_int r2) in 
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
    in rf () *. rf () *. rf ()

  );
  valid_f= (fun t -> 
    Church.is_church @@ reduce 6 (App(App(t, Church.of_int 1), Church.of_int 1))
  );
} in ga_print s; ga_steps s; *)


(** succ *)
let s = ga_init {
  pop_size= 58;
  term_len= 10;
  var_n= 6;
  gen_n=5000000;
  fitness_target= 1.0;
  test_best_f= (fun t -> 
      let r = 1 + Random.int 15 in
      try (
        let succ = reduce 4 @@ App(t, Church.of_int r) in 
        (Church.to_int succ) = r + 1)
      with | _ -> false
  );
  fitness_f= (fun t -> 
    let rf () =
      let r = 1 + Random.int 15 in
      let succ = reduce 4 @@ App(t, Church.of_int r) in 
      match succ with 
      | t' when Church.is_church t' && Church.to_int(succ) = r+1 -> 1.0
      (* | t' when Church.is_church t' && Church.to_int(succ) = r-1 -> 0.2
      | t' when Church.is_church t' && Church.to_int(succ) = r+2 -> 0.2
      | t' when Church.is_church t' && Church.to_int(succ) = r-2 -> 0.2
      | t' when Church.is_church t' && Church.to_int(succ) = r -> 0.12 *)
      | t' when Church.is_church t' -> 0.1
      | _ -> 0.0
    in rf () *. rf () *. rf () *. rf ()

  );
  valid_f= (fun t -> Church.is_church @@ reduce_fix (App(t, Church.of_int 1)));
} in ga_print s; ga_steps s;
