type prefix = Q of prefix*prefix|A of int;;

let rec get_code p n = 	
	match p with
	|A k when k = n -> Some []
	|Q (p1,p2) -> let c1 = get_code p1 n in if c1 = None then 
								 let c2 = get_code p2 n in if c2 = None then 
														None 
													else let (Some lc2) = c2 in Some (1::lc2) 
							     else let (Some lc1) = c1 in Some (0::lc1)
	|_ -> None;;
