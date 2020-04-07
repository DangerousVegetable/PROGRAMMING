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

let rec shift l sh d = 
	match l with
	|Var s -> Var s
	|Index m -> if m > d then Index (m+sh) else Index m
	|App(l1,l2) -> App(shift l1 sh d, shift l2 sh d)
	|Abs(s,l) -> Abs(s, shift l sh (d+1));;

let replace l n q = 
	let rec f l d = 	
		match l with	
		|Index m -> if m = n + d then shift q d 0 else Index m 
		|Var s -> Var s
		|App(l1,l2) -> App(f l1 d, f l2 d)
		|Abs(s,l) -> Abs(s,f l (d+1)) in
f l 0;;               

let is_normal l =	
	let rec f l = 
		match l with
		|App(Abs (_,_),_) -> false
		|App(l1,l2) -> (f l1)&&(f l2)
		|Abs(_,l) -> f l
		|_ -> true in
	f l;;              

let beta_reduction l =	
	let rec f l = if is_normal l then l else
		match l with
		|App(Abs(s,a),b) -> f (shift (replace a 1 (shift b 1 0)) (-1) 0)
		|App(l1,l2) -> f (App(f l1, f l2))
		|Abs(s,l) -> Abs(s,f l) 
		|_ -> l in
unrename (f (rename l));;	


let rec l_to_string l = 
	match l with 
 	|Var s -> s 
	|Index n -> string_of_int n
	|App (l1,l2) -> "("^(l_to_string l1)^" "^(l_to_string l2)^")"
	|Abs (s,l1) -> "\\"^s^"." ^ (l_to_string l1);; 


let two = Abs("f",Abs("g",App(Var "f",App(Var "f",Var "g"))));;

let plus_one = Abs("n",Abs("f",Abs("x",App(Var "f",App(App(Var "n",Var "f"),Var "x")))));;

let plus_two = Abs("n",App(plus_one,App(plus_one,Var "n")));;

let lam = App(plus_two,two);;

print_string (l_to_string(beta_reduction lam));;






















