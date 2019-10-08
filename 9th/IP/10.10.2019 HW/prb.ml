open String;;

let in_chan = open_in "config.txt";;

let ip_to_int s = 
	let rec f n k d m = if n >= length s then if d = 3 then let num = (int_of_string m) in if num >= 0 && num < 256 then (num lor (k lsl 8)) 
															else failwith "Incorrect ip" 
											       else failwith "Incorrect ip" 
					     else 
		match s.[n] with	
		|'.' -> let num = (int_of_string m) in if num >= 0 && num < 256 then f (n+1) (num lor (k lsl 8)) (d+1) "" else failwith "Incorrect ip"
		|a -> f (n+1) k d (m^(make 1 a)) in
f 0 0 0 "";;             

let int_to_ip m = 
	let rec f n k = if k < 4 then let l = (string_of_int (n land 0b11111111)) in if k = 3 then l 
											    else (f (n lsr 8) (k+1))^"."^l 
				 else "" in
	if 0 <= m && m < 4294967296 then f m 0 else failwith "given number is incorrect";;

let rec _data () = 
	let rec f () =
		try let ipl = ip_to_int(input_line in_chan) in ipl::f()
		with End_of_file -> [] in	

	let ip = ip_to_int(input_line in_chan) in();
	
	let mask = ip_to_int(input_line in_chan) in ();                        

	let iplist = f () in (); 
	
	let newip = ip_to_int(Sys.argv.(1)) in 
	
	if (newip land mask) = ip then if (List.exists (fun x -> x = newip) iplist) then print_string "Наш адрес" 
										    else print_string "Левый адрес"
				  else print_string "Внешний адрес";;

_data();;