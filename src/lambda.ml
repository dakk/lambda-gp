(** A lambda term *)
type term = 
	| Var of string 
	| Abs of (string * term) 
	| App of term * term
;;


(** Helper function *)
let rec to_string t = match t with
    Var x -> x
  | Abs (x,t') -> "(λ" ^ x ^ "." ^ to_string t' ^ ")"
  | App (t0,t1) -> "(" ^ (to_string t0) ^ " " ^ (to_string t1) ^ ")"
;;
  

(** Set definition *)
type 'a set = Set of 'a list;;
 
let emptyset = Set [];;
 
let rec member x s = match s with
    Set [] -> false
  | Set (y::s') -> x=y || (member x (Set s'));;
 
let rec union s t = match s with
    Set [] -> t
  | Set (x::s') -> (match union (Set s') t with Set t' ->
      if member x t then Set t' else Set (x::t'));;
 
let rec diff s x = match s with
    Set [] -> s
  | Set (y::s') -> (match diff (Set s') x with Set t' ->
      if x=y then Set s' else Set (y::t'));;


(** Return a set of free variables *)
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
let is_redex t = match t with 
    App(Abs(x,t0),t1) -> true
  | _ -> false;;
 
let rec has_redex t = match t with 
    Var x -> false
  | Abs(x,t') -> has_redex t'
  | App(t0,t1) -> is_redex t || has_redex t0 || has_redex t1;;
 
 
let rec reduce1 t = if not (has_redex t) then t else match t with
    Abs(x,t') -> Abs(x,reduce1 t')
  | App(Abs(x,t0),t1) -> subst x t1 t0 
  | App(t0,t1) -> if has_redex t0 then App(reduce1 t0,t1) else App(t0,reduce1 t1);;
 
let rec reduce t k = if k=0 then t else let t' = reduce1 t in reduce t' (k-1);;
 
let rec reduce_fix t = let t' = reduce1 t in if t'=t then t' else reduce_fix t';;


let rec len t = match t with
  Var _ -> 1
| Abs (_, t') -> 1 + len t'
| App (t', t'') -> 1 + len t' + len t''
;;