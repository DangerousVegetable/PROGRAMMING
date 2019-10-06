open String;;

let ip_to_int s = 
	let rec f n k d m = if n >= length s then if d = 3 then let num = (int_of_string m) in if num >= 0 && num < 256 then (num lor (k lsl 8)) 
															else failwith "Incorrect ip" 
											       else failwith "Incorrect ip" 
					     else 
		match s.[n] with	
		|'.' -> let num = (int_of_string m) in if num >= 0 && num < 256 then f (n+1) (num lor (k lsl 8)) (d+1) "" else failwith "Incorrect ip"
		|a -> f (n+1) k d (m^(make 1 a)) in
f 0 0 0 "";;

let s = read_line();;

print_int (ip_to_int s);; 