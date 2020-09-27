let m = read_int();;
let n = read_int();;

let rec f m n =
	let rec g n m = 
		if m = 1 then false else
			if n >= m then 
				true 
				  else if m mod n = 0 then false else g (n+1) m in

	let rec p m = 
		if m > n then 0 else if g 2 m then 1+(p (m+1)) else p (m+1) in
	p m;;

print_float ((float_of_int(f m n))/.(float_of_int(n-m+1)));;