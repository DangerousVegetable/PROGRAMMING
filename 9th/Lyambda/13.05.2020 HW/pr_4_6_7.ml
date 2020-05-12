let to_bin k n = 
	let rec f n k = 
		if n = 0 then [] else if (k mod 2) = 0 then (false::(f (n-1) (k/2))) else true::(f (n-1) (k/2)) in

	List.rev (f n k);;

let get_fun k n = to_bin k ((1 lsl n));;	

(*List.iter (fun x -> Printf.printf "%b; " x) (get_fun 2 4);;             *)

let table k n =
	let rec g l =	
		match l with
		|[] -> ""
		|[a] -> if a then "T" else "F"
		|a::tl -> if a then "T,"^(g tl) else "F,"^(g tl) in

	let rec f l m =   
		match l with
		|[] -> ""
		|a::tl -> "f("^(g (to_bin m n))^") = "^((fun x -> if x then "T" else "F") a)^"\n"^(f tl (m+1)) in
	f (get_fun k n) 0;;

print_string (table 2 4);; 
	