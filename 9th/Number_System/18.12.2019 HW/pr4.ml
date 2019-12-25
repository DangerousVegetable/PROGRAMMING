let string_from_3_ l = List.fold_left (fun s x -> if x = -1 then s ^ "!" else s ^ (string_of_int x)) "" (List.rev l);;    

let from_list l n = 	
	let (k,_) = List.fold_left (fun (r,i) x -> (r+x*(int_of_float ( (float_of_int n)**(float_of_int i) ) ),i+1) ) (0,0) l in k;;

let from_3_ l = from_list l 3;;

let to_3 m = 
	let rec f n k =
		if n < 3 then 
			   if n = 2 then [-1 + k;1] else if n + k = 2 then [-1;1] else [n+k]
		 	 else   	
		   	   if n mod 3 = 2 then (-1 + k)::(f (n/3) 1) else if (n mod 3 + k) = 2 then (-1)::(f (n/3) 1) else (n mod 3 + k)::(f (n/3) 0) in
	if m >= 0 then f m 0 else
	List.map (fun x -> x*(-1)) (f (-m) 0);;  


let (#+) a b = 
	let rec f l1 l2 k =
		match (l1,l2) with
		|([],[]) -> (match k with
				  |0 -> []
				  |1 -> [1]
				  |(-1) -> [-1])  
		|(_,[]) -> f l1 [0] k
		|([],_) -> f [0] l2 k
		|(a::t1,b::t2) -> match (a+b+k) with
				  |0 -> 0::(f t1 t2 0)
				  |1 -> 1::(f t1 t2 0)
				  |(-1) -> (-1)::(f t1 t2 0)
				  |2 -> (-1)::(f t1 t2 1)
				  |(-2) -> 1::(f t1 t2 (-1)) 
				  |3 -> 0::(f t1 t2 1)	
				  |(-3) -> 0::(f t1 t2 (-1)) in
f a b 0;;

let a = to_3 54;;    

let b = to_3 26;;

print_string (string_from_3_ (a #+ b));;                                                           
print_string "\n";;
print_int (from_3_ (a #+ b));;


	
