open Array;;

let v a = 
	let rec f n = 
		if n >= length a then [] else 
					 if (fold_left (fun r x -> if not r then false else if x.(n) then false else true) true a) then n::(f (n+1)) else f (n+1) in
	f 0;;


let a = [|
	  [|false;false;false|];
	  [|false;false;true|];
	  [|false;false;false|];	  	
	|];;

List.iter (fun x -> Printf.printf "%i; " x) (v a);; 
					 