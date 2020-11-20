type ('a,'b) tree = Leaf| Node of ('a*'b)*(('a,'b) tree)*(('a,'b) tree);;


let rec merge tr1 tr2 f = 
	let rec get tr (k,v) =
		match tr with
		|Leaf -> (v,Leaf,Leaf)
		|Node((n,m),l,r) -> if n = k then (f v m,l,r) else if k < n then let (nv,nl,nr) = get l (k,v) in (nv,nl,Node((n,m),nr,r)) else let (nv,nl,nr) = get r (k,v) in (nv,Node((n,m),l,nl),nr) in

	let rec mer t1 t2 = 
		match (t1,t2) with
		|(Leaf,_) -> t2
		|(_,Leaf) -> t1
		|(Node((kl,vl),ll,rl),Node((kr,vr),lr,rr)) -> if kl = kr then Node((kl,f vl vr),mer ll lr, mer rl rr) else if kl < kr 	then let (nv,l,r) = get lr (kl,vl) in Node((kl,nv),mer ll l,mer (mer rl r) (Node((kr,vr),Leaf,rr))) 
																	else let (nv,l,r) = get rr (kl,vl) in Node((kl,nv),mer (mer ll (Node((kr,vr),lr,Leaf))) l,mer rl r) in
	mer tr1 tr2;;

let rec string_of_tree tr = 
	match tr with
	|Leaf -> ""
	|Node ((n,m),l,r) -> "("^(string_of_int n)^":"^(string_of_int m)^","^(string_of_tree l)^","^(string_of_tree r)^")";;


let t1 = Node((4,4),Node((2,2),Node((1,1),Leaf,Leaf),Node((3,3),Leaf,Leaf)),Node((6,6),Node((5,5),Leaf,Leaf),Node((7,7),Leaf,Leaf)));;

let t2 = Node((5,100),Node((3,3),Node((2,2),Leaf,Leaf),Node((4,100),Leaf,Leaf)),Node((6,100),Leaf,Leaf));;
(*let t2 = Node((7,7),Node((1,1),Node((0,0),Leaf,Leaf),Node((3,100),Node((2,100),Leaf,Leaf),Node((6,6),Leaf,Leaf))),Node((9,9),Leaf,Leaf));;
*)

(*Printf.printf "%b" (mem (Node(4,Node(3,Leaf,Leaf),Node(5,Leaf,Leaf))) 6);;
*)

print_string (string_of_tree (merge t1 t2 (fun x y -> if x > y then x else y)));;
