type 'a tree = Leaf|Node of 'a*'a tree*'a tree;;

let rec left_bamboo n m =
	if n = 0 then Leaf else Node(m,left_bamboo (n-1) (m-1),Leaf);;


let rec right_bamboo n m =
	if n = 0 then Leaf else Node(m,Leaf,right_bamboo (n-1) (m+1));; 
	