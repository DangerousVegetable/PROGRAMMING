open String;;

let rec longest_str n inc= 
	try let line = input_line inc in (); let len = length line in if len > n then longest_str len inc else longest_str n inc
	with End_of_file -> n;;

let rec del_long n inc outc = 
	try let line = input_line inc in (); let len = length line in if len <> n then (output_string outc (line^"\n"); del_long n inc outc) else del_long (-1) inc outc
	with End_of_file -> close_out outc;;

let in_chan = open_in "IN";;
let out_chan = open_out "OUT";;

let maxn = (longest_str (-1) in_chan);;

seek_in in_chan 0;;

del_long maxn in_chan out_chan;;
