open String;;

type l = Var of string| App of l*l| Abs of string*l;;

let parse_l str =
	
	let s = str^" " in
 
	let rec parse_var n v = 
		match s.[n] with
		|a when a = '.' || a = ')' || a = ' ' -> (Var v,n)
		|a -> parse_var (n+1) (v^(make 1 a)) and

		parse_ n = 
		match s.[n] with
		|'(' -> if (n+1) < length s && s.[n+1] = '\\' then let (Var v,k) = parse_var (n+2) "" in if s.[k] = '.' then 
										    				  let (v2,k2) = parse_ (k+1) in if s.[k2] = ')' then 
																      				    (Abs (v,v2),k2+1)
																 				else 
																          			    failwith (string_of_int k2)		
											      else 
										    		   failwith (string_of_int k)
						       else
							    let (v,k) = parse_ (n+1) in if s.[k] = ' ' then 
									     				   let (v2,k2) = parse_ (k+1) in if s.[k2] = ')' then 
															        			     (App (v,v2),k2+1)
															   				 else 
																			     failwith (string_of_int k2)
												       else 
									        			   failwith (string_of_int k)
		|_ -> let (v,k) = parse_var n "" in (v,k) in
	parse_ 0;;

let rec l_to_string l = 
	match l with 
 	|Var s -> s 
	|App (l1,l2) -> "("^(l_to_string l1)^" "^(l_to_string l2)^")"
	|Abs (s,l1) -> "\\"^s^".(" ^ (l_to_string l1) ^ ")";; 

let (lya,_) = parse_l "((\\x.(\\y.z)) ab)";;

print_string (l_to_string lya);; 
		