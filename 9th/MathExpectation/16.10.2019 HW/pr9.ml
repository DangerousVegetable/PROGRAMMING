type prefix = Q of prefix*prefix|A of int;;

type bprefix = Qu of bprefix*bprefix|An of int| N;; 

let build_prefix d = 
	let rec add k l bp = 
	match (bp,l) with
	|(N,[]) -> An k
	|(N,a::b) -> if a = 0 then Qu(add k b N,N) else Qu(N,add k b N)
	|(Qu(p1,p2),a::tl) ->  if a = 0 then let newp1 = add k tl p1 in 
									(match newp1 with
									|N -> N
									|_ -> Qu(newp1,p2))
					else (let newp2 = add k tl p2 in
									(match newp2 with
									|N -> N	
									|_ -> Qu(p1,newp2)))
	|_ -> N in ();
	
	let rec make_pr bp l = 
		match l with
		|[] -> bp
		|(k,kl)::tl ->  match add k kl bp with
				|N -> N
				|newpb -> make_pr newpb tl in ();
	let rec to_pr bp = 
		match bp with 
		|N -> None
		|An k -> Some (A k)
		|Qu (p1,p2) ->  (match (to_pr p1,to_pr p2) with
				 |(None,_) -> None
				 |(_,None) -> None
				 |(Some pn1,Some pn2) -> Some (Q(pn1,pn2))) in
	to_pr (make_pr N d);;

let rec string_pr p = 
	match p with
	|A k -> string_of_int k
	|Q (p1,p2) -> "Q(" ^ (string_pr p1) ^ "," ^ (string_pr p2) ^ ")";; 

match build_prefix [(1,[0;1]);(2,[1;0]);(3,[1;1]);(7,[0;0])] with
	|None -> print_string "None"
	|Some pr -> print_string (string_pr pr);;   

 