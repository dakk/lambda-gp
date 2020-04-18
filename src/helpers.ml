let rec cumulative_apply n f = match n with
| 0 -> 1.0
| n -> (f ()) *. cumulative_apply (n-1) f
;;

let rec shuffle = function
  | [] -> []
  | [single] -> [single]
  | list -> 
    let (before, after) = List.partition (fun elt -> Random.bool ()) list in 
    List.rev_append (shuffle before) (shuffle after)
;;