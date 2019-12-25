open String;;

let parse_3 s =                     
	let rec f n r =  
		if n >= length s then r else
			match s.[n] with
			|'0' -> f (n+1) (r*3)
			|'1' -> f (n+1) (r*3+1)
			|'!' -> f (n+1) (r*3-1)
			|a -> failwith ("Unexpected Symbol:" ^ (make 1 a) ^ "," ^ (string_of_int n)) in
	f 0 0;;

print_int (parse_3 "1!0!");; (*== 17*)