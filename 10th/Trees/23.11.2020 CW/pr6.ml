type 'a tree = Leaf|Node of 'a*'a tree*'a tree;;

let rec build l = 
	let l = List.sort compare l in
	
	let rec f l d n = 
		match l with
		|m::tl -> if m = d then if tl = [] then (n+1,Node(n,Leaf,Leaf)) else let (max1,tr) = f tl (d+1) n in (max1+1,Node(max1,tr,Leaf)) else let (max1,tr) = f l (d+1) n in let (max2,tl) = f l (d+1) (max1+1) in (max2,Node(max1,tr,tl)) 
		|_ -> failwith "build failed" in

	if (List.length l > 1 && List.hd l = 0)||l = [] then None else if l = [0] then Some Leaf else let (_,t) = f l 1 0 in Some t;;



let rec string_of_tree tr = 
	match tr with
	|Leaf -> ""
	|Node (n,l,r) -> "("^(string_of_int n)^","^(string_of_tree l)^","^(string_of_tree r)^")";;


let Some t = build [1;2;4];;
print_string (string_of_tree t);;