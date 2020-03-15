open String;;

type l = Var of string| App of l*l| Abs of string*l;;

let rec (=/) a b = 
       let rec repl a s1 s2 = 
		match a with
		|Var s -> if s = s1 then Var s2 else Var s
		|App (a1,a2) -> App(repl a1 s1 s2, repl a2 s1 s2)
		|Abs (s,a) -> if s = s1 then Abs(s,a) else Abs(s,repl a s1 s2) in
	
	let counter = ref 0 in	
	let rec ($=$) a b = 	
		match (a,b) with
		|(Var a, Var b) -> a = b
		|(App (a1,a2), App (b1,b2)) -> (a1 $=$ b1)&&(a2 $=$ b2)
		|(Abs (s1,a), Abs (s2,b)) -> if s1 = s2 then a =/ b else (counter := !counter + 1; (repl a s1 ("$"^(string_of_int (!counter-1)))) $=$ (repl b s2 ("$"^(string_of_int (!counter-1))));)
		|_ -> false in
	a $=$ b ;;

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

let replace p t q =   
  	let rec f l =
		match l with
		|Var s -> if s = t then q else Var s
		|App(l1,l2) -> App((f l1),(f l2)) 
		|Abs(s,l) -> if s = t then Abs(s,l) else Abs(s,f l) in
	f p;;

let rec l_to_string l = 
	match l with 
 	|Var s -> s 
	|App (l1,l2) -> "("^(l_to_string l1)^" "^(l_to_string l2)^")"
	|Abs (s,l1) -> s^"." ^ (l_to_string l1);; 

let beta_reduction la = 
	let sl = ref None in

	let check l = 
		if !sl = None then true else let Some t = !sl in l =/ t in
	
	let rec f l =
		match l with
		|App(Abs(s,lt),a) -> if (is_free lt s a) && (check l) then (replace lt s a,true) else (sl := Some l; let (l1,b1) = f lt in if b1 then (App(Abs(s,l1),a),true) else let (l2,b2) = f a in if b2 then (App(Abs(s,lt),l2),true) else (l,false))
		|App(l1,l2) -> let (ln1,b1) = f l1 in if b1 then (App(ln1,l2),true) else let (ln2,b2) = f l2 in if b2 then (App(l1,ln2),true) else (l,false)
		|Abs(s,l1) -> let (ln1,b1) = f l1 in if b1 then (Abs(s,ln1),true) else (l,false)
		|_ -> (l,false) in

	f la;;  

let la = App(Abs("x",Abs("y",Var "x")),Var "y");;

let (bl,b) = beta_reduction la;;

Printf.printf "%b : %s" b (l_to_string bl) ;;






	