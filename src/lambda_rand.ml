open Lambda;;

let vars = "xyzwtv";;
Random.self_init ();;

let rand_var v = String.get vars (Random.int v) |> String.make 1;;


(** Generate random lambda term with a fixed length of l using v free variables *)
let rec rand_term l v = match (l, Random.int 2) with
  (0, _) -> Var (rand_var v);
| (n, 0) -> Abs (rand_var v, rand_term (l-1) v)
| (n, 1) -> App (rand_term (l-1) v, rand_term (l-1) v)
;;