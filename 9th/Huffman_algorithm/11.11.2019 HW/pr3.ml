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

let average_code_length pr =
	let rec f p l = 
		match p with
		|L x -> x*.l
		|P (p1,p2) -> (f p1 (l+.1.)) +. (f p2 (l+.1.)) in 
	f pr 0.;;  

let pref = huff_algorithm [0.5;0.175;0.325];;

print_string (string_of_float_prefix pref);;

print_string "\n";;

print_float (average_code_length pref);;

                                                                                                                                                       
