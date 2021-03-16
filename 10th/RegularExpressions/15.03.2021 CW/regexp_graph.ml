open Parse;;

let reg_graph reg = 
	let rec build reg n =
		match reg with
		|Empty -> ([|[]|], n+1, n, n)
		|Char a -> ([|[(n+1, Some a)];[]|], (n+2), n, n+1) (*vertices, edges, last index, input, output*)
		|Alt (r1,r2) -> let (e1,n1,inp1,out1) = build r1 n  in
				let (e2,n2,inp2,out2) = build r2 n1 in
				
				(*let v = Array.init ((Array.length v1) + (Array.length v2)+2) (fun i -> 	if i = 0 then Empty
													else if i-1 < (Array.length v1) then v1.(i-1)
													     else if i-1-(Array.length v1) < (Array.length v2) then v2.(i-1-(Array.length v2)) 
														  else Empty) in    *)
				(*let e = Array.init ((Array.length v1) + (Array.length v2)+2) (fun i -> 	if i = 0 then Empty
													else if i-1 < (Array.length v1) then v1.(i-1)
													     else if i-1-(Array.length v1) < (Array.length v2) then v2.(i-1-(Array.length v2)) 
														  else Empty) in     *)
				(*let v = Array.concat [v1;v2;[|Empty|];[|Empty|]] in   *)
				let e = Array.concat [e1;e2;[|[]|];[|[]|]] in
				e.(n2-n)<-[(inp1, None); (inp2, None)];
				e.(out1-n)<-e.(out1-n)@[(n2+1, None)];
				e.(out2-n)<-e.(out2-n)@[(n2+1, None)];

				(e,n2+2,n2,n2+1)
		|Concat (r1,r2) -> let (e1,n1,inp1,out1) = build r1 n in 
				   let (e2,n2,inp2,out2) = build r2 n1 in
				   
				  (* let v = Array.concat [v1;v2] in    *)
				   let e = Array.concat [e1;e2] in
				   e.(out1-n)<-e.(out1-n)@[(inp2, None)]; 
				   (e,n2,inp1,out2)
		|Range (c1,c2) -> if c1 = c2 then build (Char c1) n else 
					let alt = List.init ((Char.code c2)-(Char.code c1)+1) (fun i -> Char (Char.chr ((Char.code c1)+i))) in
					let alt = List.fold_left (fun c x -> if c  = Empty then x else Alt(c, x)) Empty alt in
					build alt n 
	in
build reg 0;;
           
(*let reg = parse "((((h|l|r))))abcd[0-1](1|3)";;
print_string (string_of_reg reg);;
print_string "\n";;
                                                                                  
let (r,_,_,_) = reg_graph reg;;

Array.iteri (fun i l -> Printf.printf "%d: " i; List.iter (fun (n,c) -> if c = None then Printf.printf "(_,%d)" n else (let Some c = c in Printf.printf "(%c,%d)" c n)) l; print_string "\n") r;;*)	
	