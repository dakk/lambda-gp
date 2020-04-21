(* we define our type system consisting of 4 types
  - an unit type  
  - product type
  - sum type
  - function types

  such that we can represent different types:
  - Boolean = Sum(Unit, Unit)
  - not boolean = Fun(Sum(Unit, Unit), Sum(Unit, Unit))
  - Natural number (4 value) as pair 2 bit = Prod(Sum(Unit,Unit),Sum(Unit,Unit))
  - Natural number (4 value) as sum of 4 unit = Sum(Sum(Unit,Unit),Sum(Unit,Unit))
  - Pairs = Prod(t1, t2)
  - Ascii char = Sum(Sum(...255 times)))
  - Ascii strings of n char = Prod(Prod(Prod(...(char, char)))
*)

type ttype = 
  | TAny
  | TUnit 
  | TSum of ttype * ttype 
  | TProd of ttype * ttype
  | TFun of ttype * ttype
;;

(* 
  since a type should have a fixed number of values (we can't represent infinite numbers 
  using this type system (I suppose), a const could assume these values:
  - unit
  - (t,t)
  - VBool (of type unit + unit)
*)

type tvalue =
  | VUnit (* of type unit *)
  | VBool of bool (* of type unit+unit *)
  | VSum of tvalue * tvalue (* of type t+t *)
  | VProd of tvalue * tvalue (* of type t*t *)
;;

type term =
  | Const of tvalue
	| Var of string
	| Abs of (string * ttype * term) 
  | App of term * term
  | Pair of term * term
  | Fst of term
  | Snd of term
  | And of term * term
  | Or of term * term
  | Not of term
  | Case of term * term * term
;;

(*
  this is an example of identity function bool -> bool
  \\y:u+u.y
  which type is u+u -> u+u

  in the same way we describe the identity function for nat(2 bit) -> nat(2 bit)
  \\y:(u+u)*(u+u).y
  which type is (u+u)*(u+u) -> (u+u)*(u+u)

  in the same way we describe the identity function for unit function
  \\y:(f:u).y
  which type is (f:u) -> (f:u)

  we introduce syntatic sugar for u+u+u+u+u = 5u

  we write few function to prettyprint a lst term:
*)

let rec type_to_string t = match t with
  TUnit -> "u"
| TAny -> "'a"
| TSum (t', t'') -> "(" ^ (type_to_string t') ^ "+" ^ (type_to_string t'') ^ ")"
| TProd (t', t'') -> "(" ^ (type_to_string t') ^ "*" ^ (type_to_string t'') ^ ")"
| TFun (t', t'') -> "(" ^ (type_to_string t') ^ "->" ^ (type_to_string t'') ^ ")"
;;

let rec const_to_string t = match t with
  VUnit -> "()"
| VBool b -> Printf.sprintf "%b" b;
| VSum(t, t') -> Printf.sprintf "(%s+%s)" (const_to_string t) (const_to_string t');
| VProd(t, t') -> Printf.sprintf "(%s,%s)" (const_to_string t) (const_to_string t');
;;

let rec to_string t = match t with
  Var x -> x
| Const t -> const_to_string t
| Abs (x, t, t') -> "(λ" ^ x ^ ":" ^ (type_to_string t) ^ "." ^ to_string t' ^ ")"
| App (t0,t1) -> "(" ^ (to_string t0) ^ " " ^ (to_string t1) ^ ")"
| Pair (t0, t1) -> "pair(" ^ (to_string t0) ^ " " ^ (to_string t1) ^ ")"
| Fst (t) -> "fst(" ^ (to_string t) ^ ")"
| Snd (t) -> "snd(" ^ (to_string t) ^ ")"
| And (t0, t1) -> (to_string t0) ^ " && " ^ (to_string t1)
| Or (t0, t1) -> (to_string t0) ^ " || " ^ (to_string t1)
| Not (t0) -> " !" ^ (to_string t0)
| Case (c, t0, t1) -> "case(" ^ (to_string c) ^ ", " ^ (to_string t0) ^ ", " ^ (to_string t1) ^ ")"
;;

to_string (Abs("x", TUnit, Const(VBool (true))));;
to_string (Abs("x", TAny, Const(VBool (true))));;


(* and now our amazing typechecker *)
exception TypeError of string;;

let rec typeof_const v = match v with 
| VUnit -> TUnit
| VBool b -> TSum(TUnit, TUnit)
| VSum(t, t') -> TSum(typeof_const t, typeof_const t')
| VProd(t, t') -> TProd(typeof_const t, typeof_const t')
;;



let rec typeof_env t bind = 
  let rec type_match_left t t' bind = (match t, t' with
    t, u when t=u -> true
  | TUnit, TUnit -> true
  | TAny, TAny -> true
  | TSum (t', t''), TSum (u', u'') -> (type_match_left t' u' bind) && (type_match_left t'' u'' bind)
  | TProd (t', t''), TProd (u', u'') -> (type_match_left t' u' bind) && (type_match_left t'' u'' bind)
  | TFun (t', t''), TFun (u', u'') -> (type_match_left t' u' bind) && (type_match_left t'' u'' bind)
  | TAny, _ -> true
  | _, _ -> false
  )
  in match t with
  | Var v -> (match List.mem_assoc v bind with 
    | true -> List.assoc v bind
    | false -> raise (TypeError ("symbol " ^ v ^ " not present in env")))
  | Const v -> typeof_const v
  | Pair (a, b) -> TProd(typeof_env a bind, typeof_env b bind)
  | Not (a) when (typeof_env a bind)=TSum(TUnit, TUnit) -> TSum(TUnit, TUnit)
  | And (a, b) when (typeof_env a bind)=TSum(TUnit, TUnit) && (typeof_env b bind)=TSum(TUnit, TUnit) -> 
    TSum(TUnit, TUnit)
  | And (a, b) when (typeof_env a bind)=TSum(TUnit, TUnit) && (typeof_env b bind)=TSum(TUnit, TUnit) -> 
    TSum(TUnit, TUnit)
  | Case (c, a, b) when (typeof_env c bind)=TSum(TUnit, TUnit) && (typeof_env a bind)=(typeof_env b bind) -> typeof_env b bind
  | Fst (p) -> (match typeof_env p bind with 
    | TProd(a,b) -> a 
    | t -> raise (TypeError ("error: fst"))
  )
  | Snd (p) -> (match typeof_env p bind with 
    | TProd(a,b) -> b
    | t -> raise (TypeError ("error: snd"))
  )
  | Abs (x, t, t') -> TFun(t, typeof_env t' @@ (x, t)::bind)
  | Case (c, a, b) -> (match typeof_env c bind with 
    | TSum(TUnit, TUnit) -> if (typeof_env a bind)<>(typeof_env b bind) then raise (TypeError ("error: case branch should have same type"))
      else typeof_env a bind
    | _ -> raise (TypeError ("error: case condition not a bool")))
  | App (t0, t1) -> (
    let t0' = typeof_env t0 bind in
    let t1' = typeof_env t1 bind in
    match t0', t1' with
    | TFun (a, b), t1' when type_match_left a t1' bind -> b
    | TAny, t1' -> TAny
    | _,_ -> raise (TypeError ("app with wrong input type " ^ type_to_string t0' ^ " " ^ type_to_string t1' ^ ""))) 
  | _ -> raise (TypeError ("error: not matched" ^ to_string t))
;;


let rec typeof t = typeof_env t [];;
let typecheck t = typeof t |> ignore; true;;


(* test type inference *)
typeof (Abs("x", TAny, Var "x"));;

(* test type inference - this should infer that x is a function 'a -> u so (u -> 'a) -> 'a*)
typeof (Abs("x", TAny, App(Var "x", Const VUnit)));;


type_to_string @@ typeof (Abs("x", TUnit, Const(VBool (true))));;

type_to_string @@ typeof (Abs("x", TUnit, Var "x"));;

(* this fails *)
type_to_string @@ typeof (App(
  Abs("x", TUnit, Var "x"),
  Const(VBool (false))
));;


(* let's now define some operations on pairs 

  o gosh, we can't define a generic fst since it needs inner types!

  a fst/snd operator should have the following type signature f:T+T-> T which is
  unfeasible with the proposed type system

  if we introduce TAny type, we may lose type consistency; or not, we introduced it
  and typeerror arises if there are problem
*)

(* the following should return type vbool (u+u) *)
type_to_string @@ typeof (App(
  Abs("x", TAny, Var "x"),
  Const(VBool (false))
));;


(* again, let's now define some operations on pairs: and again we can't;
  even if we can describe a function doing so, there is no construct able to operate
  on values; we have to define generic functions able operate.

  indeed, fst, snd for prod types, pair for creating a product
*)

type_to_string @@ typeof (Fst(Pair(Const (VBool (true)), Const (VUnit))));;
type_to_string @@ typeof (Snd(Pair(Const (VBool (true)), Const (VUnit))));;

(* 
  and other function for manipulating Sum types

  and or not and case for branching
*)

(* type error *)
type_to_string @@ typeof (Case(Const (VBool (true)), Const (VUnit), Const (VBool (false))));;

type_to_string @@ typeof (Case(Const (VBool (true)), Const (VUnit), Const (VUnit)));;





exception EvalError of string;;


let rec _eval t env = 
  let env_to_string env = List.fold_left (fun y x -> match x with | (x',x'') -> y ^ " (" ^ x' ^ "=" ^ to_string x'') "" env in
  let istrue v = match v with
  | VBool(v') -> v'
  | _ -> false
  in
  let tobool v = match v with
  | VBool(v') -> v'
  | _ -> raise (EvalError "to_bool")
  in 
  let eval_to_const t env = match _eval t env with 
    Const v -> v
  | _ -> raise (EvalError ("eval_to_const " ^ to_string t))
  in
  let toconst v = Const (v) in
  Printf.printf "=> %s (env:%s)\n" (to_string t) (env_to_string env);
  match t with
    Var x -> (try List.assoc x env with | _ -> raise (EvalError ("symbol not found " ^ x)))
  | Const t -> toconst @@ t
  | Abs (v, t, f) -> Abs(v, t, f)
  | App (t0,t1) -> 
    (match t0 with
      | Abs(x, xt, f) -> 
        let p = _eval t1 env in
        Printf.printf "env %s => %s (enving %s)\n" (env_to_string env) (env_to_string @@ (x, p)::env) x;
        _eval f ((x, p)::env)
      | _ ->  
        Printf.printf "inner res \n";
        let f = _eval t0 env in 
        Printf.printf "inner res out \n";
        _eval (App(f, t1)) env
    )
  | Pair (t0, t1) -> toconst @@ VProd(eval_to_const t0 env, eval_to_const t1 env)
  | Fst (p) -> (match eval_to_const p env with 
    | VProd(a,b) -> toconst a
    | _ -> raise (EvalError "fst on not a prod"))
  | Snd (p) -> (match eval_to_const p env with 
    | VProd(a,b) -> toconst b
    | _ -> raise (EvalError "snd on not a prod"))
  | And (t0, t1) -> 
    let a = tobool @@ eval_to_const t0 env in
    let b = tobool @@ eval_to_const t1 env in toconst @@ VBool(a && b)
  | Or (t0, t1) -> 
    let a = tobool @@ eval_to_const t0 env in
    let b = tobool @@ eval_to_const t1 env in toconst @@ VBool(a || b)
  | Not (t0) -> toconst @@ VBool (not @@ tobool @@ eval_to_const t0 env)
  | Case (c, t0, t1) when istrue (eval_to_const c env) -> _eval t0 env
  | Case (c, t0, t1) when not @@ istrue (eval_to_const c env) -> _eval t1 env
  | s -> raise (EvalError ("unexpected computation reached: " ^ to_string s))
;;

let eval t = match _eval t [] with 
| Const v -> v
| Abs(v,t,f) -> raise (EvalError ("result is a function " ^ to_string (Abs(v,t,f))))
| _ -> raise (EvalError "result does not evalute to a value");;

let eval_pp t = 
  Printf.printf "=> %s\n" @@ to_string t;
  Printf.printf "[t] <= %s\n" @@ type_to_string @@ typeof t;
  try Printf.printf "[v] <= %s\n" @@ const_to_string @@ eval t;
  with | _ -> Printf.printf "[!] <= expression evalute to a non final %s\n" (to_string @@ _eval t []);
;;

eval_pp (Case(Const (VBool(true)), Const(VBool(true)), Const(VBool(false))));;
eval_pp (Case(Const (VBool(false)), Const(VBool(true)), Const(VBool(false))));;


eval_pp (
  App (
    Abs("x", TSum(TUnit, TUnit), And(Var "x", Const (VBool(false)))),
    Const (VBool(true))
  )
);;

eval_pp (
  App (
    Abs("x", TSum(TUnit, TUnit), Var "x"),
    Const (VBool(true))
  )
);;

eval_pp (
  App (
    Abs("x", TUnit, Var "x"),
    Const (VUnit)
  )
);;


(* fail: res type is a function of a function *)
eval_pp (
    Abs("x", TSum(TUnit, TUnit), 
      Abs("y", TSum(TUnit, TUnit), And (Var "x", Var "y")))
);;


(* fail: res type is a function *)
eval_pp (
  App (
    Abs("x", TSum(TUnit, TUnit), 
      Abs("y", TSum(TUnit, TUnit), And (Var "x", Var "y"))),
    Const (VBool(true))
  )
);;


(* now that we have a solid language with a solid type system, we write the evalution function 
  recursion is achievable by writing a fixed point function
*)
let fixed_point = 
  Abs("x",TFun(TAny, TAny), 
    Abs("y",TFun(TAny, TAny),
      App(Var "y",
        App(
          App(Var "x",Var "x"),
          Var "y"
        )
      )
    )
  )
;;

typeof fixed_point;;
(* eval_pp fixed_point;; *)

let theta = App(fixed_point, fixed_point);;
(* eval_pp theta;; *)

(* we try a recursion using fixed point *)

(* let f = Abs("f", TFun(TAny, TAny), Abs("x", TUnit, Pair (Var "x", App(Var "f", Const VUnit))));;
eval_pp f;; *)
let f = Abs("f", TFun(TAny, TAny), App(Var "f", Const VUnit));;
(* eval_pp f;; *)
let app = App(theta, f);;
(* eval_pp app;; *)
eval_pp (App(app, Const VUnit));;
(* 
let t = Abs("x", TAny, Abs("y", TAny, App(Var "y", App(Var "x", Var "x"))));;
eval_pp (App(t, t));; 
*)

let t = 
  Abs("x", TFun(TAny, TSum(TUnit, TUnit)), 
    Abs("y", TSum(TUnit, TUnit), 
      Case(
        Var "y", 
        App(Var "x", Const (VBool true)),
        Var "y"
      )
    )
  )
;;

eval_pp (App(t, t));;
eval_pp (App(App(t, t), Const (VBool false)));; 
