open Lambda;;
open L;;

Random.self_init ();;

let rand_var v = L.get_var @@ Random.int v;;

let rec generate d v = match (d, Random.int 3) with
| (n, _) when n <= 1 -> Var (rand_var v);
| (_, 0) -> Abs (rand_var v, generate (d-1) v)
| (_, 1) -> 
  let a = generate (d-2) v in
  let nd = L.len a in
  App (a, generate nd v)
| (_, 2) -> Var (rand_var v);
;;

let generate_l l v = 
  let rec rin l' = match (l - l', Random.int 2) with 
  | (n, _) when n <= 0 -> (Var (rand_var v), l' + 1);
  | (_, 0) -> 
    let a = rin (l'+1) in
    (Abs (rand_var v, fst a), snd a)
  | (_, 1) -> 
    let a = rin (l'+1) in
    let b = rin ((snd a) + 1) in
    (App (fst a, fst b), snd b)
  | (_, 2) -> (Var (rand_var v), 1);
  in
  fst (rin 0)
;;
