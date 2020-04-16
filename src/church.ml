open Lambda;;

exception NotAChurchTerm;;

let rec iter f x n = if n=0 then x else App(f,(iter f x (n-1)));;

let church n = Abs("f",Abs("x",iter (Var "f") (Var "x") n));;

let rec unchurch t = match t with
  Abs(_,Abs(x,t')) -> (
    match t' with 
      Var _ -> 0
    | App(Var f,s) -> 1 + unchurch (Abs(f,Abs(x,s)))
    | _ -> raise NotAChurchTerm)
  | _ -> raise NotAChurchTerm
;;

let is_church t = try unchurch t |> ignore; true with | _ -> false;;


(* 
open Lambda_church;;

let succ = 
  Abs("n",Abs("f",Abs("x",
    App(Var "f",(App(App(Var "n",Var "f"),Var "x"))))));;
 
let iszero = 
  Abs("n",Abs("x",Abs("y",
    App(App(Var "n",Abs("z",Var "y")),Var "x"))));;
 
printf "%s\n" (to_string (reduce_fix (App(iszero,(church 0)))));;
printf "%s\n" (to_string (reduce_fix (App(iszero,(church 1)))));; *)