let name = read_line();;

let in_chan = open_in name;;

let rec word_counter n b (smb,wrds) = 
	if n >= in_channel_length in_chan then (smb,wrds) else (*seek_in in_chan n;*) let ch = input_char in_chan in 
		match ch with
		|a when not (String.contains ",./?;\':\"[]{}()1234567890!$%^&*=+-`~\\/\t \n" a) -> if b <> 0 then word_counter (n+1) (b+1) (smb,wrds) else word_counter (n+1) 1 (smb,wrds+1)
		|_ -> word_counter (n+1) 0 (smb+b,wrds);;
		(*with e -> (0,0);;*)

let (s,w) = word_counter 0 0 (0,0);;
Printf.printf "M = %f" ((float_of_int s)/.(float_of_int w));; 