let s = read_line();;

let rec am n b = 
	if n >= String.length s then 0 else 
		match s.[n] with
		|a when a = 'e'|| a = 'u' || a = 'i' || a = 'o' || a = 'a'-> if b then 1+(am (n+1) true) else am (n+1) true 
		|_ ->  am (n+1) false;;

let f () = if String.length s <= 1 then 0 else (String.length s) - 1;;

 
Printf.printf "M = %f" ((float_of_int(am 0 false))/.(float_of_int (f ())));;	