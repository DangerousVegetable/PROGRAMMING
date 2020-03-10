open String;;

type l = Var of string| App of l*l| Abs of string*l;;

let unrelated lya  = 
	let rec f l a =
		match l with
		|Var s -> if s = a then Var ("!"^a) else Var s
		|App (l1,l2) -> App (f (f l1 "") a,f (f l2 "") a)
		|Abs (s,l) -> Abs(s,f (f l s) a) in
	f lya ""                                                       
;;

let rec unrel_list l n = 
	match l with
	|Var s -> if s.[0] = '!' then ([],n+(length s - 1)) else ([(s,n)],n+(length s))
	|App (l1,l2) -> let (ll,n1) = unrel_list l1 (n+1) in let (lr,n2) = unrel_list l2 (n1+1) in (ll@lr,n2+1)
	|Abs (s,l) -> let (ll,n1) = unrel_list l (2+(length s)+n) in (ll,n1);;

let rec l_to_string l = 
	match l with 
 	|Var s -> s 
	|App (l1,l2) -> "("^(l_to_string l1)^" "^(l_to_string l2)^")"
	|Abs (s,l1) -> "\\"^s^"." ^ (l_to_string l1);; 


let lya = (Abs("xarr",Abs("t",App(Var "y",App(Var "t",Var "x")))));;

let (l,_) = unrel_list (unrelated lya) 0;;

Printf.printf "%s\n" (l_to_string lya);;

print_string (List.fold_left (fun s (_,n) -> s^(make (n - length s) ' ')^"^") "" l);; 
		 	