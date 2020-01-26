open Array;;

let if_p n = 
	let rec f k = 
		if k >= n then true else if n mod k = 0 then false else f (k+1) in
	f 2;;

let gen_p n =
	let rec f k = 
		if k > n then [] else if if_p k then k::(f (k+1)) else f (k+1) in
	f 1;;

let count n =	
	let p_l = gen_p n in
	let ar = init (n+1) (fun i -> (make (List.length p_l) 0)) in
	ar.(0).(List.length p_l - 1)<-1;
	
	let rec g k i l = 
		match l with
		|[] -> ()
		|p::tl -> if k+p <= n then (for j = i to (List.length p_l - 1) do
						ar.(k+p).(i)<-ar.(k).(j)+ar.(k+p).(i);
					    done; g k (i+1) tl) in 

						
	
	let rec f k = 
		if k = n then fold_left (+) 0 ar.(n) else (g k 0 p_l; f (k+1)) in
	f 0;;

print_int (count (read_int()));;
	   