let count n m = 
	let rec f x y =
		if x > n then f 0 (y+1) else
		if y > m then 0 else
		(n-x)*(m-y)+(f (x+1) y) in
	f 0 0;;

print_int (count 3 7);; (*m(m+1)n(n+1)/4*)
		