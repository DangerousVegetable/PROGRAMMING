exception Found of int list;;

let check n = 
	
	let rec chk down mid up l =
		match l with
		|[] -> true
		|a::tl -> if a = (down-1) || a = mid || (a = up+1) then false else chk (down-1) mid (up+1) tl in

	let rec main col placed =         
		if col >= n then raise (Found placed) else
			for i = 0 to (n-1) do
				if chk i i i placed then main (col+1) (i::placed) 
			done in
	try 
	    main 0 []; None
	with 
	    Found l -> Some l;;

match check (read_int()) with
	|None -> print_string "Impossible"
	|Some l -> print_string "Example: "; List.iter (fun x -> Printf.printf "%i," x) l;;
							
