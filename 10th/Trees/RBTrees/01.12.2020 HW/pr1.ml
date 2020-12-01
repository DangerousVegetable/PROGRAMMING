type colour = Red|Black;;

type 'a rbtree = Leaf| Node of colour*('a*'a rbtree*'a rbtree);;

let rec mem t a = 
	match t with
	|Leaf -> false
	|Node(c,(v,l,r)) -> if a = v then true else if a < v then mem l a else mem r a;;


let rotate_r t = 
	match t with
	|Node(c,(v,Node(cl,(vl,ll,lr)),r)) -> Node(cl,(vl,ll,Node(c,(v,lr,r))))
	|_ -> t;;

let rotate_l t = 
	match t with
	|Node(c,(v,l,Node(cr,(vr,rl,rr)))) -> Node(cr,(vr,Node(c,(v,l,rl)),rr))
	|_ -> t;;

let rec fixsmall t = 
	match t with
	|Node(Red,v) -> Node(Black,v)
	|_ -> t;;

let rec fix t = 
	match t with
	|Node(Black,(v,
			Node(Red,l),
			Node(Red,r)
		)) -> Node(Red,(v,Node(Black,l),Node(Black,r)))
	|Node(Black,(v,
			Node(Red,(vl,
				     Node(Red,ll),
				     lr)),
			r)) -> rotate_r (Node(Red,(v,Node(Black,(vl,Node(Red,ll),lr)),r)))
	|Node(Black,(v,
			(Node(Red,(vl,
				     ll,
				     Node(Red,lr))) as l),
			r)) -> fix(Node(Black,(v,rotate_l l,r)))
	|Node(Black,(v,
			l,
			Node(Red,(vr,
				     rl,
				     Node(Red,rr))))) -> rotate_l (Node(Red,(v,l,Node(Black,(vr,rl,Node(Red,rr))))))
	|Node(Black,(v,
			l,
			(Node(Red,(vr,
				     Node(Red,rl),
				     rr)) as r))) -> fix (Node(Black,(v,l,rotate_r r)))
	|_ -> t;;
	

let add t a = 
	let rec f t = 
		match t with
		|Leaf -> Node(Red,(a,Leaf,Leaf))
		|Node(c,(v,l,r)) -> if a = v then t else if a < v then fix(Node(c,(v,f l,r))) else fix(Node(c,(v,l,f r))) in
	fixsmall(f t);;


let string_of_colour c = 
	match c with
	|Red -> "r"
	|Black -> "b";;

let print_tree t =
        let print_prefix l = List.iter (fun b -> if b then Printf.printf "|  " else Printf.printf "   ") l in 
	 
	let rec f li t b lr = 
		match t with
		|Leaf -> print_prefix li; if lr then print_string "+--*\n" else print_string "\\--*\n"
		|Node (c,(v,l,r)) -> if b then (Printf.printf "%s%d\n" (string_of_colour c) v; f [] l false true; f [] r false false)    
					  else if lr then (print_prefix li; Printf.printf "+--%s%d\n" (string_of_colour c) v; f (li@[true]) l false true; f (li@[true]) r false false) 
					             else (print_prefix li; Printf.printf "\\--%s%d\n"(string_of_colour c) v; f (li@[false]) l false true; f (li@[false]) r false false) in
	f [] t true false;;

let tree = (Node(Black,(4,Node(Black,(2,Leaf,Leaf)),Node(Black,(6,Node(Red,(5,Leaf,Leaf)),Leaf)))));;

let tree = List.fold_left (fun t a -> add t a) tree [1;2;3;4;5;6;7;8;9;10;11;12];;
                                                                 
print_tree tree;;		

		                             

