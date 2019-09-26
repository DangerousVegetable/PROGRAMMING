let in_chan = open_in_bin "in";;
let out_chan = open_out_bin "out";;
                                                       
let to_utf8 n =	if n < 128 then [n] else  
	let rec f pow k = if ((1 lsl (8-pow)) > k) then [(((1 lsl pow) - 1) lsl (8 - pow)) lor k] else (0b10000000 lor (k mod 64))::(f (pow+1) (k lsr 6)) in
List.rev (f 1 n);;

let to_cp1251 l = 
	let rec f li = 
		match li with
		|[] -> failwith "11"
		|[a] -> (0b11111111 lsr (List.length l)) land a
		|n::tl -> (0b00111111 land n) lor ((f tl) lsl 6) in
	f (List.rev l);; 

let rec from_cp1251_to_utf8 n = if n >= in_channel_length in_chan then (close_in in_chan; close_out out_chan) else (let byte = input_byte in_chan in if byte < 128 then List.iter (fun x -> output_byte out_chan x) (to_utf8 byte) else List.iter (fun x -> output_byte out_chan x) (to_utf8 (byte+848)); from_cp1251_to_utf8 (n+1));;

let rec from_utf8_to_cp1251 n = 
	
	let rec f k = if (k lsr 7) = 0 then 0 else 1 + (f ((0b01111111 land k) lsl 1)) in	

	let rec read k = if k <= 0 then [] else (input_byte in_chan)::(read (k-1)) in

	if n >= in_channel_length in_chan then (close_in in_chan; close_out out_chan) else let byte = input_byte in_chan in (); let nz = f byte in if nz = 0 then (output_byte out_chan byte; from_utf8_to_cp1251 (n+1)) else ((output_byte out_chan (to_cp1251 (byte::(read ((f byte)-1)))-848)); from_utf8_to_cp1251 (n+(f byte)));; 

(*from_cp1251_to_utf8 0;;*) 
(*print_int (to_cp1251 [208;144]);;*)
from_utf8_to_cp1251 0;;
	  

  