type ('a,'b,'c) tree = Leaf| Node of ('a*'b*'c)*('a,'b,'c) tree*('a,'b,'c) tree;;


let rec fill_tree t =
	match t with
	|Leaf -> (0,Leaf)
	|Node((k,v,_),l,r) -> let (ld,l) = fill_tree l and (rd,r) = fill_tree r in (v+ld+rd,Node((k,v,v+ld+rd),l,r));;

let sum k1 k2 t = 	
	let rec f bl br t = 
		match t with
		|Leaf -> 0
		|Node((k,v,d),l,r) -> if k = k1 then v+(f true false r) else
					if k = k2 then v+(f false true l) else
						if bl && br then d else 
						if k < k1 then f bl br r else
						if k > k2 then f bl br l else
						v + (f bl true l) + (f true br r) in
	f false false t;;

let t = Node(("Ded",3,0),Node(("Boris",5,0),Node(("Alex",6,0),Leaf,Leaf),Node(("Chris",10,0),Leaf,Leaf)),Node(("Yak",4,0),Node(("Koreh",50,0),Leaf,Leaf),Node(("Zaez",100,0),Leaf,Leaf)));;
let (_,t) = fill_tree t;;

print_int (sum "Chris" "Koreh" t);; 
     
