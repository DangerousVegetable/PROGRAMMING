open Array;;

let exchange n coins = 
      	let m = make (n+1) 0 in

	let rec g k = if k > n then m.(n) else (m.(k) <- List.fold_left (fun x y -> if x = 0 then y else if y = 0 then x else if x < y then x else y) 0 (List.map (fun x -> if k - x = 0 then 1 else if k - x > 0 && m.(k-x) <> 0 then m.(k-x)+1 else 0) coins); g (k+1)) in

	g 0;;

print_int(exchange (read_int()) [2;3]);;

                                                                 	
