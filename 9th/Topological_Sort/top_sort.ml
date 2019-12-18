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

		  
let a = [|[|false;true;false;true;true|];
	  [|false;false;true;false;false|];
	  [|false;false;false;true;false|];
	  [|false;false;false;false;true|];
	  [|false;false;false;false;false|];
	|];;

Array.iter (fun x -> Printf.printf "%i " x) (sort a);;

		
