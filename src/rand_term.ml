open Lambda;;

let var_alphabet = "xyzwtuvpqrsabcdefghilmno";;
let var_alphabet_len = String.length var_alphabet;;

Random.self_init ();;

let rand_var_bound v =  String.get var_alphabet (Random.int v) |> String.make 1;;

let rand_var v = 
  let rec in_rand r =
    match r with
    | n when n < String.length var_alphabet -> String.get var_alphabet r |> String.make 1
    | _ -> 
      (String.get var_alphabet @@ r mod var_alphabet_len |> String.make 1) ^
      (in_rand @@ r / var_alphabet_len)
  in in_rand @@ Random.int v
;;

let rec generate d v = match (d, Random.int 3) with
| (0, _) -> Var (rand_var v);
| (_, 0) -> Abs (rand_var v, generate (d-1) v)
| (_, 1) -> App (generate (d-1) v, generate (d-1) v)
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
