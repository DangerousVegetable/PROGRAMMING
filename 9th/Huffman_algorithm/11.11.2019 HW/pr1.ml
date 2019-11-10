type 'a prefix = P of ('a prefix) * ('a prefix)| L of 'a;;

let rec huff_algorithm l = 

	let sortedl = List.sort (fun (_,a) (_,b) -> compare a b) l in
 
	    let f pl = 
		match pl with 
		|[] -> failwith "Empty list"
		|[a] -> [a]
		|(p1,h1)::(p2,h2)::tl -> huff_algorithm ((P (p1,p2),h1+h2)::tl) in 
	f sortedl;;

let rec string_of_char_prefix pr =  
	match pr with
	|L c -> String.make 1 c
	|P (p1,p2) -> "!-("^(string_of_char_prefix p1)^","^(string_of_char_prefix p2)^")";;

List.iter (fun (p,h) -> print_string ("("^(string_of_char_prefix p)^","^(string_of_int h)^")")) (huff_algorithm [(L 'x',10);(L 'y',12);(L 'z',4)]);; 