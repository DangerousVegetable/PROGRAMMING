let f n =
	let rec g p h =
		if p = 2*n then if h = 0 then 1 else 0 
		else 	if h = 0 then g (p+1) (h+1) 
			else g (p+1) (h+1) + g (p+1) (h-1) in
	g 0 0;;

print_int (f 3);;
