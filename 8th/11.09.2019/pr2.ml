let rec vow_am s n = 
	if n >= String.length s then 0 else 
	match s.[n] with 
	|a when a = 'e' || a = 'u' || a = 'i'|| a = 'o'|| a = 'a' || a = 'u' -> 1+(vow_am s (n+1)) 
	|_ -> (vow_am s (n+1));;

let s = read_line();;

Printf.printf "M = %f" ((float_of_int (vow_am s 0))/.(float_of_int (String.length s)));;