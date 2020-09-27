let is_capital n = if (n >= 65 && n<=90) || (n >= 1040 && n<=1071) then true else false;;

let to_capital n = if is_capital n then n else n-0x20;;
let to_lower n = if not (is_capital n) then n else n+0x20;;

let downgrade l = List.map (fun x -> to_lower x) l;;
let norm l = 
	match l with
	|[] -> []
	|a::b -> a::(downgrade b);;

let up l = 
	match l with
	|[] -> []
	|a::b -> (to_capital a)::(downgrade b);;
                                                
let is_proper l = 
	match l with
	|[] -> true
	|a::b -> is_capital a;;

let rec ($=) l1 l2 = 
	match (l1,l2) with
	|([],[]) -> true          
	|(hd::tl,hd2::tl2) -> if (to_capital hd = to_capital hd2) then tl $= tl2 else false
	|_ -> false;;

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


let rec (<!) l1 l2 = 
	match (l1,l2) with
	|([],_) -> true
	|(_,[]) -> false
	|(a::b,c::d) -> if to_capital a = to_capital c then b <! d else to_capital a < to_capital c;;

let stats in_chan ln =
	let tbl = Hashtbl.create 2000 in
	
	let add_word w q (*beginning?*) =
		let nrm = norm w
		(*and down = downgrade w*) in
		if Hashtbl.mem tbl (nrm,q) then 
			let num = Hashtbl.find tbl (nrm,q) in
			Hashtbl.replace tbl (nrm,q) (num+1)  
		else Hashtbl.add tbl (nrm,q) 1 in

	let rec am n =
		if (n land 0b10000000) = 0 then 0 else 1 + (am ((n land 0b01111111) lsl 1)) in 

	let rec read n = 
		if n <= 0 then [] else (input_byte in_chan)::read (n-1) in

	let from_utf8 () = let byte = input_byte in_chan in (utf8_to_num (byte::(read ((am byte) - 1)))) in 
	
	let rec read_word () = let num = from_utf8 () in if not ((num >= 65 && num <= 90) || (num >= 97 && num <= 122) || (num>=1040 && num<=1103)) then if num <> 32 && num <> 44 then ([],true) else ([],false) else try let (endw,q) = read_word () in ((num::endw),q) with _ -> ([num],true) in

	let read_word_safe () = try Some (read_word ()) with _ -> None in

	let rec f q =   
		let w = read_word_safe() in 
		match w with
		|None -> ()
		|Some (w,p) -> if w = [] then f (p||q) else (if List.length w >= ln then add_word w (q&&(is_proper w)); f p) in
		
	f true;
	Hashtbl.iter (fun (w,q) n -> if q then if (not (Hashtbl.mem tbl (downgrade w,false))) && (Hashtbl.mem tbl (w,false)) then (Hashtbl.remove tbl (w,q); Hashtbl.replace tbl (w,false) ((Hashtbl.find tbl (w,false)) + n))) tbl;
	tbl
	(*Seq.fold_left (fun l a -> a::l) [] (Hashtbl.to_seq tbl)*);;


let rec total_words in_chan = 
	let st = List.of_seq (Hashtbl.to_seq (stats in_chan 1)) in
	List.fold_left (fun s (_,n) -> s+n) 0 st;; 

let dirstats dir =
	Sys.chdir dir;
	let file_names = Array.to_list (Sys.readdir ".") in
	let n = float_of_int(List.length file_names) in
	List.fold_left (fun s name -> let i = open_in_bin name in s+.(float_of_int(total_words i))/.n) 0. file_names;;

print_float (dirstats "Dir");;

 


