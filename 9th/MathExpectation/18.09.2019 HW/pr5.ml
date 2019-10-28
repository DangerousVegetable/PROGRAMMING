let maxdeepmexp num = 
	let rec mexp n =
	
		let rec f k = 
	        	let rec g l1 l2 = 
				match l2 with 
				|[] -> []
				|a::b -> (List.map (fun x -> if x+1 > a then x+1 else a) l1)@(g l1 b) in
		
			if k > (n-1) then 
				  	 [] 
			     	     else 
				 	 let l1 = mexp k and l2 = mexp (n-1-k) in (g l1 l2)@(f (k+1)) in
				        
				
 
		if n = 0 then                    
			     [0] 
			 else 
			     f 0 in
	let lm = (mexp num) in let sum = (float_of_int(List.fold_left (+) 0 lm)) in sum/.(float_of_int(List.length lm));;


print_float (maxdeepmexp (read_int()));;   
			                       
