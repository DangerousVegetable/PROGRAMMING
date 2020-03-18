let is_Odd n = n (fun x -> x (fun f x -> x) (fun f x -> f)) (fun f x -> f);;


let rec int_to_church n = 	
	if n = 0 then (fun f x -> x) else fun f x -> f (int_to_church (n-1) f x);;

print_string ((is_Odd (fun f x -> f (f x))) "True" "False");;
