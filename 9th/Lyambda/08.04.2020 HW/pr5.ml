type l = Var of string|App of l*l| Abs of string*l|Index of int;;

let replace l n q = 
	let rec shift l sh d = 
		match l with
		|Var s -> Var s
		|Index m -> if m > d then Index (m+sh) else Index m
		|App(l1,l2) -> App(shift l1 sh d, shift l2 sh d)
		|Abs(s,l) -> Abs(s, shift l sh (d+1)) in 

	let rec f l d = 	
		match l with	
		|Index m -> if m = n + d then shift q d 0 else Index m 
		|Var s -> Var s
		|App(l1,l2) -> App(f l1 d, f l2 d)
		|Abs(s,l) -> Abs(s,f l (d+1)) in
f l 0;; 

let rec l_to_string l = 
	match l with 
 	|Var s -> s 
	|Index n -> string_of_int n
	|App (l1,l2) -> "("^(l_to_string l1)^" "^(l_to_string l2)^")"
	|Abs (s,l1) -> "\\"^s^"." ^ (l_to_string l1);; 

                                 
let lam = Abs("x",App(Index 2,Abs("y",Abs("z",App(App(Index 4,Index 1),App(Index 2, Index 1))))));;

print_string (l_to_string (replace lam 1 (Var ":D")));;
