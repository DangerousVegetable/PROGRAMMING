type l = Var of string| App of l*l| Abs of string*l;;

let related lya  = 
	let rec f l a =
		match l with
		|Var s -> if s = a then Var ("!"^a) else Var s
		|App (l1,l2) -> App (f (f l1 "") a,f (f l2 "") a)
		|Abs (s,l) -> Abs(s,f (f l s) a) in
	f lya "";;                                                       

let is_free p t q = 
	
	let q = related q in

	let rec check l a = 
		match l with
		|Var s -> if s = a then false else true
		|App (l1,l2) -> (check l1 a)&&(check l2 a)
		|Abs (s,l) -> if s = a then true else check l a in
	
	let rec f l a = 
		match l with 	
		|Var s -> if s = t then check q a else true
		|App(l1,l2) -> (f l1 a)&&(f l2 a) 
		|Abs(s,l) -> (f l a)&&(f l s) in
	f (related p) "";;


Printf.printf "%b" (is_free (Abs ("y",App (Var "x", Abs("x", Var "y")))) "x" (Abs("y",Var "y")) );;
