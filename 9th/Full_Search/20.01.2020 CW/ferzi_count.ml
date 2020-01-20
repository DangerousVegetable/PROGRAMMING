let check n = 
	
	let rec chk down mid up l =
		match l with
		|[] -> true
		|a::tl -> if a = (down-1) || a = mid || (a = up+1) then false else chk (down-1) mid (up+1) tl in

	let rec main col placed =         
		if col >= n then 1 else 
			let ans = ref 0 in
			for i = 0 to (n-1) do
				if chk i i i placed then ans:= !ans + (main (col+1) (i::placed)) 
			done; !ans in
	    main 0 [];;

(*let rec f n s = 
	if n = 0 then "" else s^(f (n-1) s);;

let rec get_table l colm = 
	match l with
	|[] -> ""
	|n::tl -> let s1 = f n "[ ]" and s2 = f (colm-n-1) "[ ]" in s1^"[$]"^s2^"\n"^(get_table tl colm);; 

(*let draw_string s = 
	let rec g n = 
		if n >= String.length s then () else 
				     match s.[n] with
				     |'!' -> Sys.command "color 2"; print_char '!'; Sys.command "color 0"; g (n+1)
				     |q -> print_char q; g (n+1) in
	g 0;;*) *)	

let colnum = read_int();;

print_int (check colnum) (*with
	|None -> print_string "Impossible"
	|Some l -> print_string "Example: \n"; print_string (get_table l colnum);;*)
							
