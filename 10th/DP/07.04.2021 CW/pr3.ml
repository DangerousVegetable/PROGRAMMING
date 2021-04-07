let f n =	
	let ar = Array.init n (fun _ -> Array.make 2 0) in
	
	ar.(0).(0) <- 1;
	ar.(0).(1) <- 1;

	let rec g m = 
		if m >= n then () else
		(ar.(m).(0) <- ar.(m-1).(0) + ar.(m-1).(1);
		ar.(m).(1) <- ar.(m-1).(0);
		g (m+1)) in
	
g 1;
ar.(n-1).(0) + ar.(n-1).(1);;

print_int (f (read_int()));;

