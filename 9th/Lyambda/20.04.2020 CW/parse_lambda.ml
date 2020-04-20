open String;;

type l = Var of string|App of l*l| Abs of string*l|Index of int;;

let parse_l str =
	let str = str^";" in         
	let rec parse_brackets n =
		match str.[n] with
		|'(' -> let (l,n) = parse (n+1) in if str.[n] = ')' then (l,n+1) else failwith (string_of_int n)
		|'\\' ->let (Var s,n) = parse_var (n+1) "" in if str.[n] = '.' then let (l,n) = parse (n+1) in (Abs (s,l),n) else failwith (string_of_int n)
		|_ -> let (l,n) = parse_var n "" in (l,n)

	and parse_var n v = 
		match str.[n] with
		|a when a = '.' || a = ')' || a = ' '|| a = ';' -> (Var v,n)
		|a -> parse_var (n+1) (v^(make 1 a)) 
	
	and parse_app n l = 
		match str.[n] with
		|' ' -> let (l2,n) = parse_brackets (n+1) in parse_app n (App (l,l2)) 
		|_ -> (l,n) 
	and parse n =	
		let (l,n) = parse_brackets n in if str.[n] = ' ' then parse_app n l else (l,n) in 


 
let (l,_) = parse 0 in
l;;

let rec l_to_string l = 
	match l with 
 	|Var s -> s 
	|Index n -> string_of_int n
	|App (l1,l2) -> "("^(l_to_string l1)^" "^(l_to_string l2)^")"
	|Abs (s,l1) -> "\\"^s^"." ^ (l_to_string l1);; 


(*print_string (l_to_string (parse_l "\\x.((((x))) (\\y.(((x)) y))) x"));; 
*)