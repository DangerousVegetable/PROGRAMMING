type lambda = Var of string| App of lambda*lambda| Abs of string*lambda;;


let rec l_to_string l = 
	match l with 
 	|Var s -> s 
	|App (l1,l2) -> "("^(l_to_string l1)^" "^(l_to_string l2)^")"
	|Abs (s,l1) -> "\\"^s^"." ^ (l_to_string l1);; 



let preparation p =
	let varctr = ref 0 in

	let rec rename_l a b p = 
		match p with
		|Var s -> if s = a then Var b else Var s
		|App(l1,l2) -> App(rename_l a b l1, rename_l a b l2)
		|Abs(s,l) -> if s = a then Abs(s,l) else Abs(s,rename_l a b l) in
	let rec rename_lambda p =	
		match p with
		|Var s -> Var s
		|App(l1,l2) -> App(rename_lambda l1,rename_lambda l2)	
		|Abs(s,l) -> varctr := !varctr + 1; let nname = "$"^(string_of_int !varctr) in Abs(nname,rename_lambda (rename_l s nname l)) in
	rename_lambda p;;

let rec free_replacement p t q =
	match p with
	|Var s -> if s = t then q else Var s
	|App(l1,l2) -> App (free_replacement l1 t q, free_replacement l2 t q)
	|Abs(s,l) -> if s = t then p else Abs(s,free_replacement l t q);;

let is_normal l =	
	let rec f l = 
		match l with
		|App(Abs (_,_),_) -> false
		|App(l1,l2) -> (f l1)&&(f l2)
		|Abs(_,l) -> f l
		|_ -> true in
	f l;;

let beta_reduction l = 
	let rec step p =	
		if is_normal p then p else 			
			match p with
	    	|App(Abs(x,a),b) -> step (free_replacement a x b)
			|App(l1,l2) -> step (App(step l1, step l2))
			|Abs(a,l) -> Abs(a,step l) 
			|_ -> p in
	step (preparation l);;

let two = Abs("f",Abs("g",App(Var "f",App(Var "f",Var "g"))));;

let plus_one = Abs("n",Abs("f",Abs("x",App(Var "f",App(App(Var "n",Var "f"),Var "x")))));;

let plus_two = Abs("n",App(plus_one,App(plus_one,Var "n")));;

let lam = App(plus_two,two);;

print_string (l_to_string(beta_reduction lam));;





















