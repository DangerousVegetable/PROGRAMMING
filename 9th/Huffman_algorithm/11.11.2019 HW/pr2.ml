type 'a prefix = P of ('a prefix) * ('a prefix)| L of 'a;;

let rec huff_algorithm l = 
		
	let sortedl = List.map (fun x -> (L x,x)) l in
 
	    let rec f pl = 
		let sortedpl = List.sort (fun (_,a) (_,b) -> compare a b) pl in
		match sortedpl with 
		|[] -> failwith "Empty list"
		|[a] -> [a]
		|(p1,h1)::(p2,h2)::tl -> f ((P (p1,p2),h1+.h2)::tl) in 
	let [(p,_)] = f sortedl in
	p;;

let rec string_of_float_prefix pr =  
	match pr with
	|L c -> string_of_float c
	|P (p1,p2) -> "!-("^(string_of_float_prefix p1)^","^(string_of_float_prefix p2)^")";;

print_string (string_of_float_prefix (huff_algorithm [0.5;0.125;0.325]));;


                                                                                                                                                       
