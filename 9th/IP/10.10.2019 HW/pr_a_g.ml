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


let dhcp ip mask l = 
	let rec f li k = 	
	match li with
	|[] -> if (k land mask) = ip then k else failwith "No free IPs"
	|a::b -> if a = k then f b (k+1) else k in
f (List.sort (fun a b -> if a = b then 0 else if a < b then -1 else 1) l) (ip land mask);;

let rec _data () = 
	let rec f () =
		try let ipl = ip_to_int(input_line in_chan) in ipl::f()
		with End_of_file -> [] in	

	let ip = ip_to_int(input_line in_chan) in();
	
	let mask = ip_to_int(input_line in_chan) in ();                        

	let iplist = f () in (); 
	
	let newip = dhcp ip mask iplist in ();
	
	let out_chan = open_out "config.txt" in
		
	output_string out_chan ((int_to_ip ip)^"\n");
	output_string out_chan ((int_to_ip mask)^"\n");
	List.iter (fun x -> output_string out_chan ((int_to_ip x)^"\n")) (iplist @ [newip]);
	close_out out_chan;;

_data();;

close_in in_chan;;