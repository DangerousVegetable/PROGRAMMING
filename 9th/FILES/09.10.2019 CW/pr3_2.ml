open String;;

let in_chan = open_in_bin "IN";;
let out_chan = open_out_bin "OUT";;

let to_utf8 n =	if n < 128 then [n] else  
	let rec f pow k = if ((1 lsl (8-pow)) > k) then [(((1 lsl pow) - 1) lsl (8 - pow)) lor k] else (0b10000000 lor (k mod 64))::(f (pow+1) (k lsr 6)) in
List.rev (f 1 n);;

let from_koi8_to_utf8 n = 
	

