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


(* lsystem *)
(* 
  we use a stochastic, context-sensitive based lsystem, so we can create different random term, 
  using only env-bounded variable (the context).

  Axiom: A<[]>
  
  [A] =>
    A<E not empty>  (0.3) => Var (rand of env)
    A<E> (0.5) => λ(rand x not in E).[A]<[E U x]>
    A<E> (0.5) => ([A]<E>, [A]<E>)
*)


let generate_l2 l' v env = 
  let venv_select l = List.nth l (Random.int @@ List.length l) in
  let rec gen' l env = match l, List.length env with 
    n, le when n<1 && le>0 -> (Var (venv_select env), l-1)
  | n, le when n<1 && le=0 -> let rv = rand_var v in (Abs(rv, Var rv), l-2)
  | _, le when le=0 -> (match Random.int 2 with
      0 -> let rv = rand_var v in 
          let nt = gen' l (rv::env) in
          (Abs(rv, fst nt), (snd nt) - 1)
    | _ -> let nt1 = gen' l env in
          let nt2 = gen' (snd nt1) env in
          (App(fst nt1, fst nt2), (snd nt1) - 1))
    | _, _ -> (match Random.int 3 with
        0 -> (Var (venv_select env), l-1)
      | 1 -> let rv = rand_var v in 
            let nt = gen' l (rv::env) in
            (Abs(rv, fst nt), (snd nt) - 1)
      | _ -> let nt1 = gen' l env in
            let nt2 = gen' (snd nt1) env in
            (App(fst nt1, fst nt2), (snd nt1) - 1))
  in fst @@ gen' l' env
;;