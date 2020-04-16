open Lambda;;
open Genetic;;
open Bool;;

(** Example 1: not function *)
let s = ga_init {
  pop_size= 16;
  term_len= 8;
  var_n= 5;
  gen_n=2;
  fitness_target= 1.0;
  fitness_f= (fun t -> match (reduce 32 @@ App(t, lfalse), reduce_fix @@ App(t, ltrue)) with 
      (t', t'') when t' = lfalse && t'' = ltrue -> 0.25
    | (t', t'') when t' = ltrue && t'' = ltrue -> 0.5
    | (t', t'') when t' = lfalse && t'' = lfalse -> 0.5
    | (t', t'') when t' = ltrue && t'' = lfalse -> 1.
    | _, _ -> 0.0
  );
  valid_f= (fun t -> 
    (* log "%b" @@ has_redex @@ App(t, Bool.lfalse); 
    log "reducing %s" @@ to_string @@ App(t, Bool.lfalse);  *)
    Bool.is_bool @@ reduce 32 (App(t, Bool.lfalse))
  );
} in

ga_print s;
ga_steps s;


(** t * 2 *)
(* let s = ga_init {
  pop_size= 256;
  term_len= 16;
  var_n= 5;
  gen_n=32;
  fitness_target= 1.0;
  fitness_f= (fun t -> 
    let rf () =
      let r = 2 + Random.int 9 in
      let mul2 = reduce 32 @@ App(t, Church.church r) in 
      match mul2 with 
      | t' when Church.is_church t' -> 
        let r' = Church.unchurch mul2 in
        (* Printf.printf "%s %d %d %d\n%!" (Lambda.to_string t) r r' (r*2); *)
        (match r' with
        | n when r * 2 = n -> 1.0 
        | n when n < r -> 0.0
        | n when n > r * 3 -> 0.0
        | n -> 
          let r' = if n > r * 2 then n - r else n in
          let diff = r * 2 - r' in
          let span = r * 2 - r in
          let rr = float (span - diff) /. (float (span) +. 0.001) in
          (* Printf.printf " => %f\n%!" rr; *)
          rr)
      | _ -> 0.0
    in rf () *. rf () *. rf () *. rf ()

  );
  valid_f= (fun t -> 
    (* log "%b" @@ has_redex @@ App(t, Bool.lfalse); 
    log "reducing %s" @@ to_string @@ App(t, Bool.lfalse);  *)
    Church.is_church @@ reduce 32 (App(t, Church.church 1))
  );
} in

ga_print s; *)

