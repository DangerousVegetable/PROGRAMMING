let rec info_am l = 
	match l with 
	|[] -> 0.
	|a::b -> -.(a*.(log a)/.(log 2.)) +. (info_am b);;


let info_am_hz lhz = 
	let len = float_of_int (List.fold_left (+) 0 lhz) in
	let rec f l = 
		match l with 	
		|[] -> []
		|a::b -> ((float_of_int a)/.len)::(f b) in
	info_am (f lhz);;


Printf.printf "%f" (info_am_hz [2;5;3;10]);;