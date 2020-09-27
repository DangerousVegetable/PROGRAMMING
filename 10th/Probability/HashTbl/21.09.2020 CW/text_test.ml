(*let in_chan = open_in_bin "in";;             
let out_chan = open_out_bin "out";;      *)

type wordtree = N of (int list)*int*wordtree*wordtree|Empty;;

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

let dirstats dir = 
	Sys.chdir dir;
	let file_names = Array.to_list (Sys.readdir ".") in
	let file_stats = List.map (fun name -> let in_chan = open_in_bin name in let s = stats in_chan 1 in close_in in_chan; s) file_names in
	Sys.chdir "..";
	file_stats;;


(*let file_prob fstats w dstats =
	let rec f l = 
		match l with
		|[] -> 0
		|tbl::tl -> if Hashtbl.mem tbl (norm w,false) then let n = Hashtbl.find tbl (norm w,false) in n+(f tl) else f tl in

	let words = List.fold_left (@) [] (List.map (fun tbl -> List.of_seq (Hashtbl.to_seq tbl)) dstats) in
	let allwords = List.fold_left (@) [] (List.map (fun tbl -> List.of_seq (Hashtbl.to_seq tbl)) (fstats::dstats)) in	

	let num = List.fold_left (fun s ((_,b),n) -> if b then s else s+n) 0 words in
	let allnum = List.fold_left (fun s ((_,b),n) -> if b then s else s+n) 0 allwords in
       
	let wnum = f dstats in
	let allwnum = f (fstats::dstats) in
	0.5*.(float_of_int wnum)/.(float_of_int num)/.((float_of_int allwnum)/.(float_of_int allnum));; *)

let test_file fstats dstats = 

	let un tbl1 tbl2 = 		
		let tb = Hashtbl.create 2000 in
		let rec f l = 
			match l with
			|[] -> ()
			|((w,b),n)::tl -> (if Hashtbl.mem tb (w,b) then Hashtbl.replace tb (w,b) ((Hashtbl.find tb (w,b))+n) else Hashtbl.add tb (w,b) n); f tl in

		let l1 = List.of_seq (Hashtbl.to_seq tbl1) in
		let l2 = List.of_seq (Hashtbl.to_seq tbl2) in
		f l1; f l2;
		tb in			
	
	let rec f l w = 
		match l with
		|[] -> 0
		|tbl::tl -> if Hashtbl.mem tbl (norm w,false) then let n = Hashtbl.find tbl (norm w,false) in n+(f tl w) else f tl w in

        let words = List.fold_left (@) [] (List.map (fun tbl -> List.of_seq (Hashtbl.to_seq tbl)) dstats) in
	let allwords = List.fold_left (@) [] (List.map (fun tbl -> List.of_seq (Hashtbl.to_seq tbl)) (fstats::dstats)) in	

	let num = List.fold_left (fun s ((_,b),n) -> if b then s else s+n) 0 words in
	let allnum = List.fold_left (fun s ((_,b),n) -> if b then s else s+n) 0 allwords in
      

	let sts = List.of_seq (Hashtbl.to_seq fstats) in
	let fnum = List.fold_left (fun s ((_,b),n) -> if b then s else s+n) 0 sts in 

	let diffwords = List.of_seq (Hashtbl.to_seq (List.fold_left (fun tb x -> un tb x) (Hashtbl.create 0) dstats)) in

	List.fold_left (fun s ((w,b),n) -> if b then s else 
								let wnum = f dstats w in
								let n = f [fstats] w in
								let allwnum = f (fstats::dstats) w in
								(*let p = (float_of_int wnum)/.(float_of_int allwnum) in *)
	                                                        (*let p = sqrt((float_of_int wnum)/.(float_of_int num)*.((float_of_int n)/.(float_of_int fnum))) in *)
								(*let p = 2.*.((float_of_int wnum)/.(float_of_int num)*.((float_of_int n)/.(float_of_int fnum)))/.((float_of_int wnum)/.(float_of_int num)+.((float_of_int n)/.(float_of_int fnum))) in
								*)
								(*let p = 1.-.sqrt( ((float_of_int wnum)/.(float_of_int num) -.(float_of_int n)/.(float_of_int fnum))*.((float_of_int wnum)/.(float_of_int num) -.(float_of_int n)/.(float_of_int fnum))) in*)
								let p = 1.*.(float_of_int wnum)/.(float_of_int num)/.((float_of_int allwnum)/.(float_of_int allnum)) in
								s+.p*.(float_of_int n)/.(float_of_int fnum)	(*let p = file_prob fstats w dstats in s+.p*.(float_of_int n)/.num*)) 0. diffwords;;



	(*let m = List.fold_left (fun s ((w,b),n) -> if b then s else 
								let wnum = f dstats w in
								let allwnum = f (fstats::dstats) w in
	                                                        let p = 1.*.(float_of_int wnum)/.(float_of_int num)/.((float_of_int allwnum)/.(float_of_int allnum)) in
								s*.((1.-.p)/.p)	(*let p = file_prob fstats w dstats in s+.p*.(float_of_int n)/.num*)) 1. sts in (* in
	m/.(m+.n*)1+(1./.m)*);;

let dstats = dirstats "Examples";;
                
let in_chan = open_in_bin "in";;
let fstats = stats in_chan 1;;

print_float (test_file fstats dstats);;

(*let l = dirstats "Corpus";;
(*let wrd = [1090;1072;1084];; (*там*)  *)
let wrd = [1073;1099];; (*бы*)
let file_name = "in1";;

let rec file_prob w name dstats =
	let rec f l num total = 
		match l with
		|[] -> if total = 0 then 0. else (float_of_int num)/.(float_of_int total)
		|(file,tbl)::tl -> if Hashtbl.mem tbl (w,false) then let n = Hashtbl.find tbl (w,false) in if file = name then f tl n (total+n) else f tl num (total+n) else f tl num total in
	f dstats 0 0;;

print_float (file_prob wrd file_name l);;  
*)