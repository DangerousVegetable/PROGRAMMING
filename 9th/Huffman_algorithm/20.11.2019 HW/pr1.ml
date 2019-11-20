open String;;

type prefix = P of (prefix)*(prefix)|L|N;;

let build_prefix l = 
	let rec add e p = 
		match (e,p) with
		|([],N) -> L
		|(0::tl,N) -> P(add tl N,N)
		|(1::tl,N) -> P(N,add tl N)
		|(0::tl,P(p1,p2)) -> let r = add tl p1 in if r = N then N else P(r,p2)
		|(1::tl,P(p1,p2)) -> let r = add tl p2 in if r = N then N else P(p1,r)
		|_ -> N in
	let rec f p pl = 
		match pl with
		|[] -> p
		|hd::tl -> let r = add hd p in if r = N then N else f r tl in
	f N l;;

let is_prefix p = 
	match (build_prefix p) with
	|N -> false
	|_ -> true;;
let is_perfect_prefix p =
	let rec f p = 
		match p with
		|N -> false
		|P(p1,p2) -> (f p1)&&(f p2)
		|L -> true in
	f (build_prefix p);;

Printf.printf "%b\n" (is_prefix [[1;0;1;0];[0;0];[1;0;0;0;0;1];[0;1]]);; 		
	 
Printf.printf "%b" (is_perfect_prefix [[0;1];[0;0];[1;1];[1;0]]);; 		
	 