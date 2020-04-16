open Lambda;;

exception InvalidChurchError;;

let rec iter f x n = if n=0 then x else App(f,(iter f x (n-1)));;

let church n = Abs("f",Abs("x",iter (Var "f") (Var "x") n));;

let rec unchurch t = match t with
  Abs(f,Abs(x,t')) -> (match t' with 
    Var x -> 0
  | App(Var f,s) -> 1 + unchurch (Abs(f,Abs(x,s)))
  | _ -> raise InvalidChurchError)
  | _ -> raise InvalidChurchError;;