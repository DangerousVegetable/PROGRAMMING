let hf s n =
	let rec f l = 
		match l with
		|[] -> 0
		|a::tl -> ((f tl)*11+a) mod n in
	f s;; 

let to_utf8 n =	if n < 128 then [n] else  
	let rec f pow k = if ((1 lsl (8-pow)) > k) then [(((1 lsl pow) - 1) lsl (8 - pow)) lor k] else (0b10000000 lor (k mod 64))::(f (pow+1) (k lsr 6)) in
List.rev (f 1 n);;

let utf8_to_num l = 
	let rec f li = 
		match li with
		|[] -> failwith "11"
		|[a] -> (0b11111111 lsr (List.length l)) land a
		|n::tl -> (0b00111111 land n) lor ((f tl) lsl 6) in
	f (List.rev l);; 

let stats1 in_chan =
	let tbl = Hashtable.create 2000 hf in
	
	let add_word w = let v = Hashtable.find tbl w in 
		match v with
		|None -> Hashtable.add tbl w 1 
		|Some n -> Hashtable.add tbl w (n+1) in

	let rec am n =
		if (n land 0b10000000) = 0 then 0 else 1 + (am ((n land 0b01111111) lsl 1)) in 

	let rec read n = 
		if n <= 0 then [] else (input_byte in_chan)::read (n-1) in

	let from_utf8 () = let byte = input_byte in_chan in (utf8_to_num (byte::(read ((am byte) - 1)))) in 
	
	let rec read_word () = let num = from_utf8 () in if num <= 64 then [] else try let endw = read_word () in (num::endw) with _ -> [num] in            

	let read_word_safe () = try Some (read_word ()) with _ -> None in

	let rec f () =   
		let w = read_word_safe() in 
		match w with
		|None -> ()
		|Some w -> if w != [] then ignore (add_word w); f() in
		
	f ();;

let stats2 in_chan =
	let tbl = Hashtable1.create 2000 hf in
	
	let add_word w = let v = Hashtable1.find tbl w in 
		match v with
		|None -> Hashtable1.add tbl w 1 
		|Some n -> Hashtable1.add tbl w (n+1) in

	let rec am n =
		if (n land 0b10000000) = 0 then 0 else 1 + (am ((n land 0b01111111) lsl 1)) in 

	let rec read n = 
		if n <= 0 then [] else (input_byte in_chan)::read (n-1) in

	let from_utf8 () = let byte = input_byte in_chan in (utf8_to_num (byte::(read ((am byte) - 1)))) in 
	
	let rec read_word () = let num = from_utf8 () in if num <= 64 then [] else try let endw = read_word () in (num::endw) with _ -> [num] in            

	let read_word_safe () = try Some (read_word ()) with _ -> None in

	let rec f () =   
		let w = read_word_safe() in 
		match w with
		|None -> ()
		|Some w -> if w != [] then ignore (add_word w); f() in
		
	f ();;

let stats3 in_chan =
	let tbl = Hashtable2.create 2000 hf in
	
	let add_word w = let v = Hashtable2.find tbl w in 
		match v with
		|None -> Hashtable2.add tbl w 1 
		|Some n -> Hashtable2.add tbl w (n+1) in

	let rec am n =
		if (n land 0b10000000) = 0 then 0 else 1 + (am ((n land 0b01111111) lsl 1)) in 

	let rec read n = 
		if n <= 0 then [] else (input_byte in_chan)::read (n-1) in

	let from_utf8 () = let byte = input_byte in_chan in (utf8_to_num (byte::(read ((am byte) - 1)))) in 
	
	let rec read_word () = let num = from_utf8 () in if num <= 64 then [] else try let endw = read_word () in (num::endw) with _ -> [num] in            

	let read_word_safe () = try Some (read_word ()) with _ -> None in

	let rec f () =   
		let w = read_word_safe() in 
		match w with
		|None -> ()
		|Some w -> if w != [] then ignore (add_word w); f() in
		
	f ();;


let in_chan = open_in_bin "in";;             
let time1  = Sys.time();;
stats1 in_chan;;
let time2 = Sys.time();;
close_in in_chan;;
Printf.printf "Open-type hash:%f\n" (time2-.time1);;


let in_chan = open_in_bin "in";; 
let time1  = Sys.time();;
stats2 in_chan;;
let time2 = Sys.time();;
close_in in_chan;;
Printf.printf "1st Close-type hash:%f\n" (time2-.time1);;

let in_chan = open_in_bin "in";; 
let time1  = Sys.time();;
stats3 in_chan;;
let time2 = Sys.time();;
close_in in_chan;;
Printf.printf "2nd Close-type hash:%f\n" (time2-.time1);;






