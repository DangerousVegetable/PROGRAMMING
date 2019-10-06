open String;;

let int_to_ip m = 
	let rec f n k = if k < 4 then let l = (string_of_int (n land 0b11111111)) in if k = 3 then l 
											    else (f (n lsr 8) (k+1))^"."^l 
				 else "" in
	if 0 <= m && m < 4294967296 then f m 0 else failwith "given number is incorrect";;

print_string (int_to_ip (read_int()));;  