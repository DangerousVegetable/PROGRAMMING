type tree = Tree of tree list;;

let tree_of_string s = 
	let rec parse n = 
		if n >= String.length s then ([],n) else
			match s.[n] with
			|'(' -> let (tl1,n) = parse (n+1) in if s.[n] = ')' then let (tl2,n) = parse (n+1) in ((Tree tl1)::tl2,n) else failwith ((string_of_int n) ^ " - unexpected \')\'")
			|')' -> ([],n)
			|_ -> failwith "Unknown symbol" in
	let (s,_) = parse 0 in s;;

let rec string_of_tree tl = 
	match tl with
	|[] -> ""
	|(Tree tl)::ttl -> "("^(string_of_tree tl)^")"^(string_of_tree ttl);; 

let s = read_line();;

print_string (string_of_tree (tree_of_string s));; 