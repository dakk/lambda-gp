open Lambda;;

let rec cumulative_apply n f = match n with
| 0 -> 1.0
| n -> (f ()) *. cumulative_apply (n-1) f
;;

let rec cumulative_apply_i n f = match n with
| 0 -> 1.0
| n -> (f n) *. cumulative_apply_i (n-1) f
;;

let rec shuffle = function
  | [] -> []
  | [single] -> [single]
  | list -> 
    let (before, after) = List.partition (fun elt -> Random.bool ()) list in 
    List.rev_append (shuffle before) (shuffle after)
;;

let rec check_env_unbound t env = match t with 
  L.Var x -> if not (List.mem x env) then failwith ("Unbound variable " ^ x) else true
| Abs(x, f) -> check_env_unbound f (x::env)
| App(f, p) -> check_env_unbound p env
;;

let check_term_bound t = try check_env_unbound t [] with | _ -> false;;


let rec fix_term_unbound_env t env = match t with 
  L.Var x -> if not (List.mem x env) then (
    if (List.length env)=0 then t else L.Var (List.nth env (Random.int @@ List.length env))
  ) else t
| Abs(x, f) -> Abs(x, f) (* x, fix_term_unbound_env f (x::env)) *)
| App(f, p) -> App(f, fix_term_unbound_env p env)
;;

let fix_term_unbound t = fix_term_unbound_env t [];;