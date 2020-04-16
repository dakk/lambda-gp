open Lambda;;
open Genetic;;
open Bool;;

(** Example 1: not function *)
let s = ga_init {
  pop_size= 16;
  term_len= 10;
  var_n= 3;
  gen_n=32;
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


(* let r = Rand_term.generate_l 64 4;;
log "%s" (to_string r);;
log "\t=> %s" (to_string (reduce_fix r));; *)

