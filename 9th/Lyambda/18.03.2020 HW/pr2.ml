let print_church n = print_int (n ((+)1) 0);;

let rec int_to_church n = 	
	if n = 0 then (fun f x -> x) else fun f x -> f (int_to_church (n-1) f x);; 

print_church (int_to_church 10);;