open String;;

let rec hash s n = if n >= length s then 0 else Char.code(s.[n]) + (hash s (n+1));;

let search s subs = 
 
	let hash0 = hash subs 0 in
	let rec find h n = 
			if (length s - n) >= (length subs) then if n = 0 then(
									if h = hash0 then( if (sub s n (length subs)) = subs then n 
										  	  else find h (n+1))
							    		else find h (n+1))  
							        else(
									let h = h - Char.code(s.[n-1]) + Char.code(s.[n + (length subs) - 1]) in
										if h = hash0 then (if (sub s n (length subs)) = subs then n 
								  				  else find h (n+1))
										else find h (n+1))
			else failwith "No subs in a given string" in 
find (hash (sub s 0 (length subs)) 0) 0;;   


print_int(search "abcdefg" "defg");;
 
