type ('a,'b) tree = Leaf| Node of ('a*'b)*(('a,'b) tree)*(('a,'b) tree);;

let rec mem tr a = 
	match tr with
	|Leaf -> false
	|Node ((k,_),l,r) -> if k = a then true else if a < k then mem l a else mem r a;;

let rec add tr (k,v) = 
	match tr with
	|Leaf -> Node((k,v),Leaf,Leaf)
	|Node((n,m),l,r) -> if n = k then tr else if k < n then Node((n,m),add l (k,v),r) else Node((n,m),l,add r (k,v));;

let rec delete tr a =
	let rec find_right tr = 
		match tr with
		|Leaf -> None
		|Node(a,_,Leaf) -> Some a
		|Node(_,_,r) -> find_right r in 

	let rec f tr = 
		match tr with
		|Leaf -> Leaf
		|Node((n,m),l,r) -> if n = a then 
						let right = find_right l in 
						match right with
						|None -> r
						|Some (k,v) -> Node((k,v),delete l k,r) 
				else if a < n then Node((n,m),f l,r) else Node((n,m),l,f r) in
	f tr;; 

let verify tr =
	let get_val op = 
		match op with
		|None -> failwith ""
		|Some v -> v in

	let rec f t min max = 
		match t with
		|Leaf -> true
		|Node((k,v),tl,tr) -> if (min = None || (get_val min) < k)&&(max = None || k < (get_val max)) then (f tl min (Some k))&&(f tr (Some k) max) else false in
	f tr None None;;
									

let rec string_of_tree tr = 
	match tr with
	|Leaf -> ""
	|Node ((n,m),l,r) -> "("^(string_of_int n)^":"^(string_of_int m)^","^(string_of_tree l)^","^(string_of_tree r)^")";;

let t = Node((5,5),Node((3,3),Node((2,2),Leaf,Leaf),Node((4,4),Leaf,Leaf)),Node((6,6),Leaf,Leaf));;

(*Printf.printf "%b" (mem (Node(4,Node(3,Leaf,Leaf),Node(5,Leaf,Leaf))) 6);;
*)

print_string (string_of_tree (delete t 5));;  
(*let t = Node((5,5),Node((3,3),Node((2,2),Leaf,Leaf),Node((6,6),Leaf,Leaf)),Node((8,8),Node((7,7),Leaf,Leaf),Leaf));;

Printf.printf "%b" (verify t)*);;
