open Array;;    

let sort a = 
	let res = make (Array.length a) (-1) in

	let num = ref 0 in

	let rec main n =                            
			let rec f m = 
				if m >= length res then 
							(res.(n) <- !num; num := 1 + !num) 
						   else 
							if a.(n).(m) then 
									if res.(m) = -1 then 
											    (main m; f (m+1)) 
											else f (m+1)
								     else f (m+1) in
	
	if n >= length res then () else if res.(n) = -1 then (f 0; main (n+1)) else main (n+1) in
	
	main 0;
	res;;



let number_of_ways a = 	
	let sa = sort a in
	
	let res = make (length sa) 0 in
	
	let num = ref sa.(0) in

	let rec main n = 
		let rec f m = 
			if m >= length res then () else if a.(n).(m) then (res.(m) <- res.(m) + res.(n); f (m+1)) else f (m+1) in
		let rec newn k = 
			if k >= length res then () else if sa.(k) = (!num - 1) then (num := !num - 1; main k) else newn (k+1) in
		f 0; newn 0 in
	res.(0) <- 1;
	main 0;
	res;;
	
let a = [|[|false;true;false;true;true|];
	  [|false;false;true;false;false|];
	  [|false;false;false;true;false|];
	  [|false;false;false;false;true|];
	  [|false;false;false;false;false|];
	|];;

Array.iter (fun x -> Printf.printf "%i " x) (number_of_ways a);;
	
