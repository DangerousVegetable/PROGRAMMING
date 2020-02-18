open Array;;

let count_max m = 
	let ans = init (length m + 1) (fun _ -> make (length m.(0) + 1) 0) in
	
	let rec g x y = 
		if x >= (length m.(0)) then 0 else if m.(y-1).(x) then (1+ans.(y-1).(x+1)) else g (x+1) y in	

	let rec f x y = 
		if y > (length m) then 
			      () 
			 else 
			      if x < 0 then 
					    f (length m.(0)-1) (y+1) 
				       else 
					    if x < (length m.(0)-2) then
					    	                         if m.(y-1).(x) || m.(y-1).(x+1) then 
							      	                                          (ans.(y).(x) <- 1+(ans.(y-1).(x+2)); 
													   f (x-1) y)
								 				     else                            
						  	      	    					  (let an1 = ans.(y-1).(x+2) in
													   let an2 = g (x+2) y in
													   ans.(y).(x) <- max an1 an2; 
													   f (x-1) y)													  
								    else 
								    	 if m.(y-1).(x) || (x <> (length m.(0) - 1) && m.(y-1).(x+1)) then 
																       (ans.(y).(x) <- 1; f (x-1) y)
																  else 
																       (ans.(y).(x) <- 0; f (x-1) y) in
	f (length m.(0) - 1) 1;
	ans.(length m).(0);;



let mtrx = [|
	     [|false;false;false;false;true;false|];
	     [|false;false;false;true;false;false|];
	     [|false;false;false;false;true;false|]|];;

print_int (count_max mtrx);;
	
						
													  
								 