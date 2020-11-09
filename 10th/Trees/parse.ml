type tree = Tree of tree list;;

let tree_of_string s = 
	let s = s^")" in
	let rec parse n =      
		match s.[n] with
		|'(' -> let (tl1,n) = parse (n+1) in if s.[n] = ')' then let (tl2,n) = parse (n+1) in ((Tree tl1)::tl2,n) else failwith ((string_of_int n) ^ " - unexpected \')\'")
		|')' -> ([],n)
		|_ -> failwith "Unknown symbol" in
	let (t,n) = parse 0 in 
	if n = (String.length s)-1 then t else failwith ("Incorrect tree: "^(string_of_int n));;

let rec string_of_tree tl = 
	match tl with
	|[] -> ""
	|(Tree tl)::ttl -> "("^(string_of_tree tl)^")"^(string_of_tree ttl);; 

let s = read_line();;

print_string (string_of_tree (tree_of_string s));;