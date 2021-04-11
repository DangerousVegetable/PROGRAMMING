let f n = 
	let a = Array.init (n+1) (fun i -> Array.make (i+1) 0) in
	
	a.(0).(0) <- 1;

	let rec g k l =	          	
		if k > n then () else
		if l >= k then (a.(k).(k) <- 1; g (k+1) 0) else (
			let (s,_) = Array.fold_left (fun (s,i) v -> if i < l then (s,i+1) else (s+v,i+1)) (0,0) a.(k-l-1) in a.(k).(l) <- s; g k (l+1)) in

	g 0 0;
	a.(n).(0);;

print_int (f 20);; 