type ('a,'b) t = {
	hashfunc: ('a -> int -> int);                           
	mutable data: (('a*'b) opt) array;	
} 
and 'a opt = Val of 'a|Deleted|Empty;;

let rec min_degree n = 
	let rec f m = 
		if 1 lsl m >= n then 1 lsl m else f (m+1) in
	f 0;;

let create n hashf = {hashfunc = hashf; data = Array.make (min_degree n) Empty};;

let rec update table n = 
	if n <= Array.length table.data then failwith "New size can't be applied!" else
		(let old_data = table.data in
		table.data <- Array.make (min_degree n) Empty;
		Array.iter (fun x -> 	match x with
				 	|Empty -> ()	
					|Deleted -> ()
					|Val (a,b) -> ignore (add table a b)) old_data)
		 
and add table a b =
	let hash = table.hashfunc a (Array.length table.data) in

	let rec f h n r = 
		if h = hash && r then let () = update table ((Array.length table.data)+1) in add table a b else (   
				match table.data.(h) with
				|Deleted -> f ((h+n*n) mod (Array.length table.data)) (n+1) (h = hash||r)
				|Val (k,v) -> if k = a then let () = table.data.(h) <- Val (a,b) in Some v else f ((h+n*n) mod (Array.length table.data)) (n+1) (h = hash||r)
				|Empty -> let () = table.data.(h) <- Val (a,b) in None) in
	f hash 1 false;;
		  
	

let mem table a = 
	let hash = table.hashfunc a (Array.length table.data) in
	let rec f h n r = 
		if h = hash && r then false else (   
				match table.data.(h) with
				|Deleted -> f ((h+n*n) mod (Array.length table.data)) (n+1) (h = hash||r)
				|Val (k,v) -> if k = a then true else f ((h+n*n) mod (Array.length table.data)) (n+1) (h = hash||r)
				|Empty -> false) in
	f hash 1 false;;
	
let find table a = 
	let hash = table.hashfunc a (Array.length table.data) in
	let rec f h n r = 
		if h = hash && r then None else (   
				match table.data.(h) with
				|Deleted -> f ((h+n*n) mod (Array.length table.data)) (n+1) (h = hash||r)
				|Val (k,v) -> if k = a then Some v else f ((h+n*n) mod (Array.length table.data)) (n+1) (h = hash||r)
				|Empty -> None) in
	f hash 1 false;;
	
let delete table a = 
	let hash = table.hashfunc a (Array.length table.data) in
	let rec f h n r = 
		if h = hash && r then None else (   
				match table.data.(h) with
				|Deleted -> f ((h+n*n) mod (Array.length table.data)) (n+1) (h = hash||r)
				|Val (k,v) -> if k = a then let () = table.data.(h) <- Deleted in Some v else f ((h+n*n) mod (Array.length table.data)) (n+1) (h = hash||r)
				|Empty -> None) in
	f hash 1 false;;

let iter table f = 
	Array.iter (fun l -> match l with 
			     |Empty -> ()
			     |Deleted -> ()
			     |Val l -> f l) table.data;;

let fold table f s =     
	Array.fold_left (fun a l -> match l with
				    |Empty -> a
				    |Deleted -> a
				    |Val l -> f a l) s table.data;;

