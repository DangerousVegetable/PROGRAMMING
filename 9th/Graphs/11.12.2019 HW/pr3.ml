open Array;;

let stats a = 
	let rec f n = 
		if n >= length a then [] else 
					 	let inp = (fold_left (fun r x -> if x.(n) then r+1 else r) 0 a) in 
					 	let outp = (fold_left (fun r x -> if x then r+1 else r) 0 a.(n)) in
						(n,inp,outp)::(f (n+1)) in
	f 0;;

let a = [|
	  [|false;true;true|];
	  [|false;false;true|];
	  [|false;false;false|];	  	
	|];;


List.iter (fun (x,i,o) -> Printf.printf "%i - IN:%i|OUT:%i; " x i o) (stats a);; 

