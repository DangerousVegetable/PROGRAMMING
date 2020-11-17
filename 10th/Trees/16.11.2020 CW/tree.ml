type 'a tree = Leaf| Node of 'a*('a tree)*('a tree);;

let rec mem tr a = 
	match tr with
	|Leaf -> false
	|Node (b,l,r) -> if b = a then true else if a < b then mem l a else mem r a;;

let rec add tr a = 
	match tr with
	|Leaf -> Node(a,Leaf,Leaf)
	|Node(n,l,r) -> if n = a then tr else if a < n then Node(n,add l a,r) else Node(n,l,add r a);;

let rec delete tr a =
	let rec find_right tr = 
		match tr with
		|Leaf -> None
		|Node(a,_,Leaf) -> Some a
		|Node(_,_,r) -> find_right r in 

	let rec f tr = 
		match tr with
		|Leaf -> Leaf
		|Node(n,l,r) -> if n = a then 
						let right = find_right l in 
						match right with
						|None -> r
						|Some a -> Node(a,delete l a,r) 
				else if a < n then Node(n,f l,r) else Node(n,l,f r) in
	f tr;; 
									

let rec string_of_tree tr = 
	match tr with
	|Leaf -> ""
	|Node (n,l,r) -> "("^(string_of_int n)^","^(string_of_tree l)^","^(string_of_tree r)^")";;

let t = Node(5,Node(3,Node(2,Leaf,Leaf),Node(4,Leaf,Leaf)),Node(6,Leaf,Leaf));;

(*Printf.printf "%b" (mem (Node(4,Node(3,Leaf,Leaf),Node(5,Leaf,Leaf))) 6);;
*)

print_string (string_of_tree (delete t 5));;
