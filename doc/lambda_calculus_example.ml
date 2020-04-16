(** 
 * Reference code from https://dokuwiki.unica.it/doku.php?id=untypedlambda-prog
 *)

type term = 
	| Var of string 
	| Abs of (string * term) 
	| App of term * term
;;



let id = Abs("x",Var "x");;
let k = Abs("x",Abs("y",Var "x"));;
let omega = App(Abs("x",App(Var "x",Var "x")),Abs("x",App(Var "x",Var "x")));;
let const z = Abs("x",Var z);;



let rec string_of_term t = match t with
    Var x -> x
  | Abs (x,t') -> "\\" ^ x ^ "." ^ string_of_term t'
  | App (t0,t1) -> "(" ^ (string_of_term t0) ^ " " ^ (string_of_term t1) ^ ")";;
  
  
  
type 'a set = Set of 'a list;;
 
let emptyset = Set [];;
 
let rec member x s = match s with
    Set [] -> false
  | Set (y::s') -> x=y or (member x (Set s'));;
 
let rec union s t = match s with
    Set [] -> t
  | Set (x::s') -> (match union (Set s') t with Set t' ->
      if member x t then Set t' else Set (x::t'));;
 
let rec diff s x = match s with
    Set [] -> s
  | Set (y::s') -> (match diff (Set s') x with Set t' ->
      if x=y then Set s' else Set (y::t'));;

let rec fv t = match t with
    Var x -> Set [x]
  | App(t0,t1) -> union (fv t0) (fv t1)
  | Abs(x,t0) -> diff (fv t0) x;;


(** Substitution *)
let count = ref(-1);;
let gensym = fun () -> count := !count +1; "x" ^ string_of_int (!count);;
 
let rec subst x t' t = match t with
    Var y -> if x=y then t' else Var y
  | App(t0,t1) -> App(subst x t' t0, subst x t' t1)
  | Abs(y,t0) when y=x -> Abs(x,t0)
  | Abs(y,t0) when y!=x && not (member y (fv t')) -> Abs(y, subst x t' t0)
  | Abs(y,t0) when y!=x && member y (fv t') -> 
      let z = gensym() in Abs(z,subst x t' (subst z (Var y) t0));;
      
      
(** Reduction *)
let isredex t = match t with 
    App(Abs(x,t0),t1) -> true
  | _ -> false;;
 
let rec hasredex t = match t with 
    Var x -> false
  | Abs(x,t') -> hasredex t'
  | App(t0,t1) -> isredex t or hasredex t0 or hasredex t1;;
 
exception Error;;
 
let rec reduce1 t = if not (hasredex t) then t else match t with
    Abs(x,t') -> Abs(x,reduce1 t')
  | App(Abs(x,t0),t1) -> subst x t1 t0 
  | App(t0,t1) -> if hasredex t0 then App(reduce1 t0,t1) else App(t0,reduce1 t1);;
 
let rec reduce t k = if k=0 then t else let t' = reduce1 t in reduce t' (k-1);;
 
let rec reducefix t = let t' = reduce1 t in if t'=t then t' else reducefix t';;
 
reduce (App((const "z"),omega)) 1;;

(** Booleans *)
let ift = Abs("x",Var "x");;
let t = Abs("x",Abs("y",Var "x"));;
let f = Abs("x",Abs("y",Var "y"));;


(** Churc numerals *)
let rec iter f x n = if n=0 then x else App(f,(iter f x (n-1)));;
 
let church n = Abs("f",Abs("x",iter (Var "f") (Var "x") n));;
 
print_string (string_of_term (church 0));;
print_string (string_of_term (church 1));;
print_string (string_of_term (church 2));;
 
let succ = 
  Abs("n",Abs("f",Abs("x",
    App(Var "f",(App(App(Var "n",Var "f"),Var "x"))))));;
 
let iszero = 
  Abs("n",Abs("x",Abs("y",
    App(App(Var "n",Abs("z",Var "y")),Var "x"))));;
 
reducefix (App(iszero,(church 0)));;
reducefix (App(iszero,(church 1)));;
 
let rec unchurch t = match t with
  Abs(f,Abs(x,t')) -> (match t' with 
    Var x -> 0
  | App(Var f,s) -> 1 + unchurch (Abs(f,Abs(x,s)))
  | _ -> raise Error)
  | _ -> raise Error;;
 
unchurch (church 2);;
 
unchurch (reducefix (App(succ,(church 1))));;

(** Pairs *)
let pair = Abs("x",Abs("y",Abs("z",App(App(Var "z",Var "x"),Var "y"))));;
let fst = Abs("x",App(Var "x",t));;
let snd = Abs("x",App(Var "x",f));;
 
let t1 = App(snd,App(App(pair,church 0),church 1));;
 
unchurch (reducefix t1);;

(** Pred *)
let z = App(App(pair,church 0),church 0);;
let s = Abs("x",App(App(pair,App(snd,Var "x")),App(succ,App(snd,Var "x"))));;
 
unchurch (reducefix (App(snd,App(s,App(s,z)))));;
 
let pred = Abs("n",App(fst,App(App(Var "n",s),z)));;
 
unchurch (reducefix (App(pred,church 9)));;

(** Fixed points *)
let u = Abs("x",Abs("y",App(Var "y",App(App(Var "x",Var "x"),Var "y"))));;
let theta = App(u,u);;

(** ABS *)
let fadd = Abs("f",Abs("n",Abs("m",App(App(App(ift,App(iszero,Var "n")),Var "m"),App(succ,App(App(Var "f",App(pred,Var "n")),Var "m"))))));;
let add = App(theta,fadd);;
unchurch (reducefix (App(App(add,church 3),church 5)));;


