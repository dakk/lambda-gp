(* lst with unit const *)

type ttype = 
  | TUnit
  | TFun of ttype * ttype
  | TRecFun of ttype
;;

type term = 
  | Unit
	| Var of string 
	| Abs of (string * ttype * term) 
  | App of term * term
;;

(* these are types for bool and church numbers *)
let tnat = TFun (TFun(TUnit, TUnit), TFun (TUnit, TUnit));;
let tbool = TFun (TUnit, TFun (TUnit, TUnit));;

let rec type_to_string tt = match tt with
  (* first two are just sugar for better representation *)
    TFun (TFun(TUnit, TUnit), TFun(TUnit, TUnit)) -> "nat"
  | TFun (TUnit, TFun (TUnit, TUnit)) -> "bool"
  | TUnit -> "unit"
  | TFun (t, t') -> "(" ^ type_to_string t ^ " → " ^ type_to_string t' ^ ")"
  | TRecFun (t) -> let tt = type_to_string t in "(" ^ tt ^ " → ...)"
;;

let rec to_string t = 
  let rec unchurch t = match t with
    Abs(f,ft,Abs(x,xt,t')) -> (match t' with 
      Var x -> 0
    | App(Var f,s) -> 1 + unchurch (Abs(f,TUnit, Abs(x, TUnit, s))))
  in match t with
    Var x -> x 
  | Abs(x, TUnit, Abs(y,TUnit, Var x')) when x = x' && x <> y -> "true"
  | Abs(x, TUnit, Abs(y,TUnit, Var y')) when y = y' && x <> y -> "false"
  | Unit -> "()"
  | Abs (x,xt,t') -> (try (Printf.sprintf "%d" @@ unchurch t) with | _ -> "(λ" ^ x ^ ":" ^ type_to_string xt ^ "." ^ to_string t' ^ ")")
  | App (t0,t1) -> "(" ^ (to_string t0) ^ " " ^ (to_string t1) ^ ")"
;;

exception TypeError of string;;

let rec typeof_env t env = 
  let rec inner_trec t = match t with
  | TFun(a, _) -> inner_trec a
  | TRecFun(a) -> a
  | _ -> t
  in
  let type_eq t t' env = match t, t' with 
  | TRecFun(a), b -> a=(inner_trec b)
    | _, _ -> t=t'
  in
  match t with
    Var v -> (match List.mem_assoc v env with 
    | true -> List.assoc v env
    | false -> raise (TypeError ("symbol " ^ v ^ " not present in env")))
  | Unit -> TUnit
  | Abs (x, t, t') -> TFun(t, typeof_env t' @@ (x, t)::env)
  | App (f, p) -> 
    Printf.printf "%s (%s)\n" (type_to_string @@ typeof_env f env) (type_to_string @@ typeof_env p env);
    (match typeof_env f env, typeof_env p env with
    | TFun (it, ot), it' when type_eq it it' env -> ot
      (* Printf.printf "%s %s\n" (type_to_string @@ it)  (type_to_string @@ it'); ot *)
    | TFun (it, ot), it' when not (type_eq it it' env) -> raise (TypeError ("type mismatch; expected " ^ type_to_string it ^ " got " ^ type_to_string it'))
    | TRecFun(it), it' when it'=TRecFun(it) -> TRecFun(it)
    | TRecFun(it), it' -> raise (TypeError ("rec type mismatch " ^ type_to_string it ^ " but " ^ type_to_string it'))
    | _ -> raise (TypeError ("cannot apply: not a function"))
  )
;;

let typeof t = typeof_env t [];;
let typecheck t = try typeof_env t [] |> ignore; true with | _ -> false;;

  
let var_alphabet = "xyzfnwtuvpqrsabcdeghilmo";;
let var_alphabet_len = String.length var_alphabet;;


let get_var_bound n =  String.get var_alphabet n |> String.make 1;;

let get_var v = 
  let rec in_v r =
    match r with
    | n when n < String.length var_alphabet -> String.get var_alphabet r |> String.make 1
    | _ -> 
      (String.get var_alphabet @@ r mod var_alphabet_len |> String.make 1) ^
      (in_v @@ r / var_alphabet_len)
  in in_v v
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
  | Unit -> Set []
  | App(t0,t1) -> union (fv t0) (fv t1)
  | Abs(x,xt,t0) -> diff (fv t0) x;;

let fv_l t = match fv t with Set l -> l;;

(** Substitution *)
let vcount = ref(-1);;
let gensym = fun () -> vcount := !vcount +1; get_var !vcount;; (* "x" ^ string_of_int (!vcount);; *)
 
let rec subst x t' t = match t with
    Var y -> if x=y then t' else Var y
  | App(t0,t1) -> App(subst x t' t0, subst x t' t1)
  | Abs(y,xt,t0) when y=x -> Abs(x,xt, t0)
  | Abs(y,xt,t0) when y!=x && not (member y (fv t')) -> Abs(y, xt, subst x t' t0)
  | Abs(y,xt,t0) when y!=x && member y (fv t') -> 
      let z = gensym() in Abs(z,xt,subst x t' (subst z (Var y) t0))
  | _ -> t
;;

(** α-conversion *)
let rec alfa_conversion a b t = match t with 
    Var x -> if x=a then Var b else Var x
  | Unit -> Unit
  | Abs(x, xt, t') -> Abs((if x=a then b else a), xt, alfa_conversion a b t')
  | App(t', t'') -> App(alfa_conversion a b t', alfa_conversion a b t'')
;;

(** η-conversion *)
let eta_conversion t = match t with
| Abs(x, xt, App(m, Var y)) when x=y && not (member x @@ fv m) -> m
| _ -> t
;;
      
(** β-reduction *)
let is_redex t = match t with 
    App(Abs(x,xt,t0),t1) -> true
  | _ -> false;;
 
let rec has_redex t = match t with 
    Abs(x,xt,t') -> has_redex t'
  | App(t0,t1) -> is_redex t || has_redex t0 || has_redex t1
  | _ -> false
;;
 
 
let rec reduce1 t = if not (has_redex t) then t else match t with
    Abs(x,xt,t') -> Abs(x,xt, reduce1 t')
  | App(Abs(x,xt,t0),t1) -> subst x t1 t0 
  | App(t0,t1) -> if has_redex t0 then App(reduce1 t0,t1) else App(t0,reduce1 t1)
;;
 
let reduce k t =
  let rec reduce' k t = if k=0 then t else let t' = reduce1 t in reduce' (k-1) t' in
  vcount := 0;
  reduce' k t
;;

let reduce_fix t =
  let rec reduce_fix' t = let t' = reduce1 t in if t'=t then t' else reduce_fix' t' in
  vcount := 0;
  typeof t |> ignore; 
  reduce_fix' t
;;


let reduce_fix_timeout ?(n=128) t = 
  let rec subst' x t' t n' = if n' = 0 then t else match t with
    Var y -> if x=y then t' else Var y
  | App(t0,t1) -> App(subst' x t' t0 (n'-1), subst' x t' t1 (n'-1))
  | Abs(y,xt,t0) when y=x -> Abs(x,xt,t0)
  | Abs(y,xt,t0) when y!=x && not (member y (fv t')) -> Abs(y, xt, subst' x t' t0 (n'-1))
  | Abs(y,xt,t0) when y!=x && member y (fv t') -> 
    let z = gensym() in Abs(z,xt,subst' x t' (subst' z (Var y) t0 (n'-1)) (n'-1))
  | _ -> t
  in
  let rec reduce1' t n' = if not (has_redex t) || n' = 0 then t else match t with
    Abs(x,xt,t') -> Abs(x,xt,reduce1' t' (n'-1))
  | App(Abs(x,xt,t0),t1) -> subst' x t1 t0 (n'-1)
  | App(t0,t1) -> if has_redex t0 then App(reduce1' t0 (n'-1),t1) else App(t0,reduce1' t1 (n'-1))
  in  
  let rec reduce_fix' t n' = 
  	let t' = reduce1' t (n'-1) in if t'=t || n' = 0 then t' else reduce_fix' t' (n'-1) 
  in
  vcount := 0;
  reduce_fix' t n
;;


let rec len t = match t with
  Var _ -> 1
| Unit -> 1
| Abs (f, _, t') -> 1 + len t'
| App (t', t'') -> 1 + len t' + len t''
;;

let dump_term t =
  Printf.printf "- : %s = %s\n" (type_to_string @@ typeof t) (to_string t)
;;


dump_term (Abs("x", TUnit, Var "y"));;

dump_term (Abs("x", TFun(TUnit, TUnit), App(Var "x", Unit)));;

let t = Abs("x", TUnit, Abs("y",TUnit, Var "x"));;
let f = Abs("x", TUnit, Abs("y",TUnit, Var "y"));;

dump_term t;;
dump_term f;;

let rec iter f x n = if n=0 then x else App(f,(iter f x (n-1)));;
let church n = Abs("f", TFun(TUnit, TUnit), Abs("x", TUnit, iter (Var "f") (Var "x") n));;

(* typeof of church is TFun (TFun (TUnit, TUnit), TFun (TUnit, TUnit)) *)
dump_term @@ church 12;;

exception Error;;

let rec unchurch t = match t with
  Abs(f,ft,Abs(x,xt,t')) -> (match t' with 
    Var x -> 0
  | App(Var f,s) -> 1 + unchurch (Abs(f,TUnit, Abs(x, TUnit, s)))
  | _ -> raise Error)
  | _ -> raise Error;;

let succ = 
  Abs("n", TFun (TFun (TUnit, TUnit), TFun (TUnit, TUnit)), 
    Abs("f", TFun (TUnit, TUnit),
      Abs("x", TUnit,
        App(Var "f",(App(App(Var "n",Var "f"),Var "x"))))));;

dump_term succ;;

(* following should fail! *)
dump_term @@ reduce_fix (App(succ, t));; 
dump_term @@ reduce_fix (App(succ, f));;

(* following should run! *)
dump_term @@ reduce_fix (App(succ, church 0));;

let iszero = 
    Abs("n",typeof @@ church 0, 
      Abs("x", TUnit,
        Abs("y", TUnit,
          App(
            App(Var "n",
              Abs("z", TUnit, Var "y")
            ),
            Var "x"
          )
        )
      )
    );; 

dump_term iszero;;

dump_term @@ reduce_fix @@ App(iszero, church 0);;
dump_term @@ reduce_fix @@ App(iszero, church 1);;
dump_term @@ reduce_fix @@ App(succ, church 1);;





let u2 = Abs("x", TRecFun(TUnit), App(Var "x", Var "x"));;
type_to_string @@ typeof u2;;


type_to_string @@ typeof @@ App(u2, u2);;


let u = 
  Abs("x", TRecFun(TFun(TUnit, TUnit)),
    Abs("y", TRecFun(TFun(TUnit, TUnit)),
      App(Var "y",
        App(
          App(Var "x",Var "x"),
          Var "y")
        )
      )
    );;

type_to_string @@ typeof u;;

let theta = App(u,u);; 
type_to_string @@ typeof theta;;

let ift = Abs("x",TUnit, Var "x");;
type_to_string @@ typeof ift;;


(** Pairs *)

let pair_of ty = 
  Abs("x", ty,
    Abs("y", ty,
      Abs("z", TFun(ty, TFun(ty, TUnit)),
        App(
          App(Var "z",Var "x"),
          Var "y")
        )
      )
    );;

dump_term @@ pair_of tnat;;

let tpair_of ty = typeof @@ pair_of ty;;

let fst ty = Abs("x", TFun(tbool, ty), App(Var "x", t));;
dump_term @@ fst tnat;;



dump_term @@ pair_of TUnit;;

let fst ty = Abs("x", tpair_of ty, App(Var "x", t));;
dump_term @@ fst TUnit;;

dump_term @@ pair_of tnat;;
dump_term @@ pair_of tbool;;

let fst ty = Abs("x", ty, App(Var "x", t));;
dump_term @@ fst tnat;;
let snd ty = Abs("x", ty, App(Var "x", f));;
dump_term @@ snd tbool;;

dump_term @@ reduce_fix @@ App(fst tbool, App(App(pair_of tbool, t), f));;
dump_term @@ reduce_fix @@ App(snd tbool, App(App(pair_of tbool, t), f));;

dump_term @@ reduce_fix @@ App(fst tnat, App(App(pair_of tnat, church 12), church 24));;
dump_term @@ reduce_fix @@ App(snd tnat, App(App(pair_of tnat, church 12), church 24));;

(* let list_empty = pair;;
let push a = App(App(list_empty, list_empty), a);;
let hd l = fst l;;
let tl l = snd l;; *)
(*  
let t1 = App(snd,App(App(pair,church 0),church 1));;
 
unchurch (reduce_fix t1);; *)

(** Pred *)
let z = App(App(pair_of tnat,church 0),church 0);;
dump_term z;;

let s = 
  Abs("x", TFun(TUnit, TUnit),
    App(
      App(
        pair_of @@ TFun(TUnit, TUnit),
        App(snd @@ TFun(TUnit, TUnit),Var "x")
      ),
      App(
        succ,
        App(snd @@ TFun(TUnit, TUnit),Var "x")
      )
    )
  );;
dump_term @@ s;;
 
unchurch (reduce_fix (App(snd tnat,App(s,App(s,z)))));;
 
let pred = Abs("n",tnat,App(fst tnat,App(App(Var "n",s),z)));; 
dump_term pred;;


(** ABS *)
(* let fadd = 
  Abs("f", TFun (TFun (TUnit, TUnit), TFun (TUnit, TUnit)),
    Abs("n", TFun (TFun (TUnit, TUnit), TFun (TUnit, TUnit)),
      Abs("m", TFun (TFun (TUnit, TUnit), TFun (TUnit, TUnit)),
        App(App(App(ift,App(iszero,Var "n")),Var "m"),App(succ,App(App(Var "f",App(pred,Var "n")),Var "m"))))));;
let add = App(theta,fadd);;
to_string (reduce_fix (App(App(add,church 3),church 5)));; *)