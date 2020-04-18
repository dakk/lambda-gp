let rec cumulative_apply n f = match n with
| 0 -> 1.0
| n -> (f ()) *. cumulative_apply (n-1) f
;;