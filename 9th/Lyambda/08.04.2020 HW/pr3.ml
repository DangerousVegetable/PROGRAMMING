type l = Var of string|App of l*l| Abs of string*l|Index of int;;

let unrename l =	
	let varcntr = ref 0 in
	
	let rec f l depth = 
		match l with
		|Var s -> Var  s
		|Index n -> if n = depth then Var ("$"^(string_of_int !varcntr)) else Index n
		|App(l1,l2) -> App(f l1 depth, f l2 depth)
		|Abs(s,l) -> Abs(s,f l (depth+1)) in
	let rec g l = 
		match l with
		|App(l1,l2) -> App(g l1,g l2)
		|Abs(s,l) -> varcntr := !varcntr + 1; let name = "$"^(string_of_int !varcntr) in Abs(name,g (f l 1))
		|_ -> l in
g l;;

let rec l_to_string l = 
	match l with 
 	|Var s -> s 
	|Index n -> string_of_int n
	|App (l1,l2) -> "("^(l_to_string l1)^" "^(l_to_string l2)^")"
	|Abs (s,l1) -> "\\"^s^"." ^ (l_to_string l1);; 

let lam = Abs("x",App(Index 1,Abs("y",Abs("z",App(App(Index 3,Index 1),App(Index 2, Index 1))))));;

print_string (l_to_string (unrename lam));;
 