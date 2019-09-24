let in_c = open_in_bin "text";;

let size = in_channel_length in_c;;

let rec f n b = if n >= size then 0	
			 else let byte = input_byte in_c in 
							if byte = 176 && b then 1 + (f (n+1) false) 
							else if byte = 208 then f (n+1) true
						   	     else f (n+1) false;;


Printf.printf "Количество букв а: %i" (f 0 false);;
						 