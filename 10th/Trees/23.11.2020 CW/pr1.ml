type 'a tree = Leaf|Node of 'a*'a tree*'a tree;;

let rec is_bamboo t = 
	match t with
	|Leaf -> true
	|Node(_,Leaf,r) -> is_bamboo r
	|Node(_,l,Leaf) -> is_bamboo l
	|_ -> false;;

Printf.printf "%b" (is_bamboo (Node(0,Node(0,Node(0,Leaf,Node(0,Leaf,Node(0,Leaf,Leaf))),Leaf),Leaf)));;