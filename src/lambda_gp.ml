open Printf;;
open Lambda;;
open Lambda_rand;;


let r = rand_term 16 6 in
printf "<lambda-gp> %s => %s\n" (to_string r) (to_string (reduce_fix r));;

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