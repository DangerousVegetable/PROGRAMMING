let out_chan = open_out_bin "out";;
                                                       
let to_utf8 n =	if n < 128 then [n] else  
	let rec f pow k = if ((1 lsl (8-pow)) > k) then [(((1 lsl pow) - 1) lsl (8 - pow)) lor k] else (0b10000000 lor (k mod 64))::(f (pow+1) (k lsr 6)) in
List.rev (f 1 n);;



let rec jap_num k = 
        let rec f n = if n <= 10 then ( 
					match n with 
					|0 -> []
				   	|1 -> [0x4e00]
		    	 		|2 -> [0x4e8c]
					|3 -> [0x4e09]
					|4 -> [22235]
					|5 -> [0x4e94]
					|6 -> [0x516d]
					|7 -> [0x4e03]
					|8 -> [0x516b]
					|9 -> [0x4e5d]
					|10 -> [0x5341] )else if n >= 20 then (f (n/10))@[0x5341]@(f (n mod 10)) else [0x5341]@(f (n mod 10)) in 	
	if k = 100 then close_out out_chan else (List.iter (fun x -> List.iter (fun y -> output_byte out_chan  y) (to_utf8 x)) (f k); output_byte out_chan 32; jap_num (k+1));;

jap_num 1;;
	
