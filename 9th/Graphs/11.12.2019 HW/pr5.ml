let rec from_num k n = 
	if k < n then [k] else (k mod n)::(from_num (k/n) n);;

List.iter (fun x -> Printf.printf "(%i)" x) (from_num 17 3);;