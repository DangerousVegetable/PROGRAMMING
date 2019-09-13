let s = read_line();;


let rec chrl str = 
	let rec add li c = 
		match li with 
		|[] -> [(c,1)]
		|(a,n)::b-> if a = c then (a,n+1)::b else if (Char.code c) < (Char.code a) then (c,1)::li else (a,n)::(add b c)in

	let rec f n l = if n >= (String.length s) then l else f (n+1) (add l str.[n]) in
	
	f 0 [];;

List.iter (fun (x,y) -> Printf.printf "(%c,%i);" x y) (chrl s);;