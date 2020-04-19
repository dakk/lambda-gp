open OUnit2;;

let dummy () octx = assert_equal true true;;

let tlist = [
	"dummy" >:: dummy ();
];;