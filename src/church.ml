open Lambda;;

exception NotAChurchTerm;;

let rec iter f x n = if n=0 then x else App(f,(iter f x (n-1)));;

let of_int n = Abs("f",Abs("x",iter (Var "f") (Var "x") n));;

let rec to_int t = match t with
  Abs(_,Abs(x,t')) -> (
    match t' with 
      Var _ -> 0
    | App(Var f,s) -> 1 + to_int (Abs(f,Abs(x,s)))
    | _ -> raise NotAChurchTerm)
  | _ -> raise NotAChurchTerm
;;

let convert_to_canonical t = 
  let rec convin t = match t with
    App(Var f, inner) -> App(Var "f", convin inner)
  | App(Var f, Var x) -> App(Var "f", Var "x")
  | Var x -> Var "x"
  in match t with 
  | Abs (f, Abs (x, inner)) -> Abs("f", Abs("x", convin inner))
  | _ -> raise NotAChurchTerm
;;

(* convert_to_canonical @@ Abs ("y", Abs ("z", Var "a"));;
convert_to_canonical @@ Abs ("z", Abs ("c", App (Var "b", App (Var "l", App (Var "a", Var "t")))));; *)


let is_church t = try to_int t |> ignore; true with | _ -> false;;

let is_church2 t = try is_church (convert_to_canonical t) with | _ -> false;;
let to_int2 t = to_int (convert_to_canonical t);;


(* 
open Lambda_church;;

let succ = 
  Abs("n",Abs("f",Abs("x",
    App(Var "f",(App(App(Var "n",Var "f"),Var "x"))))));;
 
let is_zero = 
  Abs("n",Abs("x",Abs("y",
    App(App(Var "n",Abs("z",Var "y")),Var "x"))));;
 
printf "%s\n" (to_string (reduce_fix (App(is_zero,(church 0)))));;
printf "%s\n" (to_string (reduce_fix (App(is_zero,(church 1)))));; 


*)