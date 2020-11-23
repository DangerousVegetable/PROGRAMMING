type 'a tree = Leaf|Node of 'a*'a tree*'a tree;;

let is_balanced t = 
	let rec f t = 
		match t with
		|Leaf -> (0,true)
		|Node(_,l,r) -> let (dl,bl) = f l and (dr,br) = f r in if dr = dl || dr-dl = 1 || dl - dr = 1 then (1+max dl dr,bl&&br) else (1 + max dl dr,false) in
	
	let (_,b) = f t in
	b;;

Printf.printf "%b" (is_balanced (Node(0,Node(0,Node(0,Leaf,Leaf),Node(0,Leaf,Leaf)),Node(0,Node(0,(*Node(0,Leaf,Leaf)*)Leaf,Leaf),Leaf))));;