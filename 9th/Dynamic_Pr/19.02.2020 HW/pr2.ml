open Array;;

let count_max m = 
	let ans = init (length m + 1) (fun _ -> make (length m.(0) + 1) 0) in

	let path = init (length m + 1) (fun _ -> make (length m.(0) + 1) []) in

	(*let rec path_list (x,y) =  
		match path.(y).(x) with
		|(a,b) -> if a >= length m.(0) || b < 0 then [(x,y)] else (x,y)::path_list (a,b) in *)			
	
	let rec g x y l = 
		if x >= (length m.(0)) then (0,[]) else if m.(y-1).(x) then (1+ans.(y-1).(x+1),l@[(x,y-1)]@path.(y-1).(x+1)) else g (x+1) y (l@[(x,y)]) in	

	let rec f x y = 
		if y > (length m) then 
			      () 
			 else 
			      if x < 0 then 
					    f (length m.(0)-1) (y+1) 
				       else 
					    if x < (length m.(0)-1) then
					    	                         if m.(y-1).(x) || m.(y-1).(x+1) then 
							      	                                          (ans.(y).(x) <- 1+(ans.(y-1).(x+2));
													   path.(y).(x) <- ((x,y-1)::(x+1,y-1)::path.(y-1).(x+2)); 
													   f (x-1) y)
								 				     else                            
						  	      	    					  (let an1 = ans.(y-1).(x+2) in
													   let (an2,al) = g (x+2) y [] in
													   ans.(y).(x) <- max an1 an2;
													   if an1 >= an2 then                            
															     path.(y).(x) <- ((x,y-1)::(x+1,y-1)::path.(y-1).(x+2))
														    	else (
															     path.(y).(x) <- al); 
													   f (x-1) y)													  
								    else 
								    	 if m.(y-1).(x) then 
											     (ans.(y).(x) <- 1; path.(y).(x) <- [(x,y-1)]; f (x-1) y)
											else 
						 				             (ans.(y).(x) <- 0; path.(y).(x) <- [(x,y-1)]; f (x-1) y) in
	f (length m.(0) - 1) 1;
	(ans.(length m).(0),path.(length m).(0));;



let mtrx = [|
	     [|false;false;false;false;true;|];
	     [|false;false;false;true;false;|];
	     [|false;false;false;false;true;|]|];;

let (n,l) = (count_max mtrx);;

print_int n;;
print_string "\n";;
List.iter (fun (x,y) -> Printf.printf "(%i,%i);" x y) l;;
             
	
						
													  
								 