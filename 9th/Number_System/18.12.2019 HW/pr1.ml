let to_3 m = 
	let rec f n k =
		if n < 3 then 
			   if n = 2 then [-1 + k;1] else if n + k = 2 then [-1;1] else [n+k]
		 	 else   	
		   	   if n mod 3 = 2 then (-1 + k)::(f (n/3) 1) else if (n mod 3 + k) = 2 then (-1)::(f (n/3) 1) else (n mod 3 + k)::(f (n/3) 0) in
	if m >= 0 then f m 0 else
	List.map (fun x -> x*(-1)) (f (-m) 0);;  


let num = (to_3 (-17)) in

List.iter (fun x -> Printf.printf "(%i)" x) num;;                       


