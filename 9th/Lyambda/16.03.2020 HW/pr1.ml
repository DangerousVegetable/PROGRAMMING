open String;;

type l = Var of string| App of l*l| Abs of string*l;;

let is_beta l =	
	match l with
	|App (Abs _,_) -> true
	|_ -> false;;