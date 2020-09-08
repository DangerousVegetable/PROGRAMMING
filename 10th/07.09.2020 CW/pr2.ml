let in_chan = open_in_bin "in";;
let out_chan = open_out "out";;

let rec f n = 
	let rec g m = 
		if m > n then () else (output_string out_chan ((String.make m 'A')^" "); g (m+1)) in
	g 0;;  

f (read_int ());;
close_out out_chan;;