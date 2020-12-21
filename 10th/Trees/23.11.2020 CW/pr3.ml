type 'a tree = Leaf|Node of 'a*'a tree*'a tree;;

let rec minmax t = 
	match t with                   
	|Leaf -> (0,0)
	|Node (_,l,r) -> let (minl,maxl) = minmax l and (minr,maxr) = minmax r in (1+min minl minr,1+max maxl maxr);;


let t = Node(0,Node(0,Node(0,Leaf,Node(0,Leaf,Node(0,Leaf,Leaf))),Leaf),Leaf);; 
let (m,n) = minmax t;;
Printf.printf "min: %d, max: %d" m n;;