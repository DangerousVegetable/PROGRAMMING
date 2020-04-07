type l = Var of string|App of l*l| Abs of string*l|Index of int;;


let rename lam = 
	let rec f lam a n = 
		match lam with
		|Var s -> if s = a then Index n else Var s
		|Index _ -> lam
		|App (l1,l2) -> let l1 = f l1 a n in let l2 = f l2 a n in App (l1,l2)
		|Abs (s,l) -> if s = a then lam else let l = f l a (n+1) in Abs (s,l) in
	let rec g lam = 
		match lam with
		|App(l1,l2) -> App(g l1, g l2)
		|Abs (s,l) -> let l = f l s 1 in Abs(s,g l) 
		|_ -> lam in
	g lam;;

let rec l_to_string l = 
	match l with 
 	|Var s -> s 
	|Index n -> string_of_int n
	|App (l1,l2) -> "("^(l_to_string l1)^" "^(l_to_string l2)^")"
	|Abs (s,l1) -> "\\"^s^"." ^ (l_to_string l1);; 

let lam = Abs("x",Abs("y",Abs("z",App(App(Var "x",Var "z"),App(Var "y", Var "z")))));;

print_string (l_to_string (rename lam));;

		 