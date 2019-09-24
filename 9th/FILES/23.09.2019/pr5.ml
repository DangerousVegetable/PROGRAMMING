let in_c = open_in_bin "text";;

let size = in_channel_length in_c;;

let rec f n b = if n >= size then 0	
			 else let byte = input_byte in_c in                       
				match byte with
				|226 -> f (n+1) 226
				|186 -> if b = 226 then f (n+1) 186 else (f (n+1) (-1))
				|169 -> if b = 186 then 1+(f (n+1) 169) else (f (n+1) (-1))
				|_ -> (f (n+1) (-1));;
				


Printf.printf "Количество китайских королей: %i" (f 0 (-1));;
						 