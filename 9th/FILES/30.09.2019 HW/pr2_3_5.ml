let in_chan = open_in_bin "in";;
let out_chan = open_out "out";;

type wordtree = N of (int list)*int*wordtree*wordtree|Empty;;

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
	|(a::b,c::d) -> if a = c then b <! d else a < c;;

let rec add t l = 
	match t with
	|Empty -> N (l,1,Empty,Empty)
	|N (l1,k,tl,tr) -> if l1 = l then N (l,(k+1),tl,tr) else if l1 <! l then N (l1,k,add tl l,tr) else N (l1,k,tl,add tr l);;

let rec top n t = 
	let rec merge l1 l2 = 
		match (l1,l2) with
		|([],l2) -> l2
		|(l1,[]) -> l1
		|((la,a)::b,(lc,c)::d) -> if a >= c then (la,a)::(merge b l2) else (lc,c)::(merge l1 d) in  
	
	let rec cut n l = if n = 0 then [] else 
		match l with
		|[] -> [] 
		|a::b -> a :: (cut (n-1) b) in

	match t with
	|Empty -> []
	|N (l,k,tl,tr) -> cut n (merge (merge (top n tl) (top n tr)) [(l,k)]);;

let top_words ln ntop =
	let rec am n =
		if (n land 0b10000000) = 0 then 0 else 1 + (am ((n land 0b01111111) lsl 1)) in 

	let rec read n = 
		if n <= 0 then [] else (input_byte in_chan)::read (n-1) in

	let from_utf8 n = let byte = input_byte in_chan in (utf8_to_num (byte::(read ((am byte) - 1))),(max ((am byte)-1) 0) + n) in 
	
	let rec read_word n = let (num,endn) = from_utf8 n in if num <= 64 then ([],endn) else let (endw,endn) = read_word (endn+1) in ((num::endw),endn) in

	let rec f t n = if n >= (in_channel_length in_chan) then top ntop t else let (w,endn) = read_word n in if (List.length w >= ln) then f (add t w) (endn+1) else f t (endn+1) in

	f Empty 0;;

Printf.fprintf out_chan "%s" (List.fold_left (fun a (x,k) -> a^"(["^(List.fold_left (fun a x -> a^(string_of_int x)^";") "" x)^"],"^(string_of_int k)^"])\n") "" (top_words 3 50));;      
		                      


(*print_int (utf8_to_num [0b01111111]);; *)
