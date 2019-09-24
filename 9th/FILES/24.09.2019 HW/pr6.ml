let out_chan = open_out_bin "out";;
                                                       
let to_utf8 n =	if n < 128 then [n] else  
	let rec f pow k = if ((1 lsl (8-pow)) > k) then [(((1 lsl pow) - 1) lsl (8 - pow)) lor k] else (0b10000000 lor (k mod 64))::(f (pow+1) (k lsr 6)) in
List.rev (f 1 n);;

let pr_l l = List.iter (fun x -> Printf.printf "%i " x) l;;

(*pr_l (to_utf8 (read_int()));;*)

let rec rus_al n k = if k = 0 then close_out out_chan else (List.iter (fun x -> output_byte out_chan x) (to_utf8 n); rus_al (n+1) (k-1));;

rus_al 0x410 32;;
	
