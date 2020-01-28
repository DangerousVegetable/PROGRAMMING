open Array;;

let exchange n coins = 
      	let m = make (n+1) [] in

	let g () = Array.iteri (fun i x -> List.iter (fun t -> if x <> [] && t+i <= n then m.(i+t) <- m.(t)@x) coins) m; if m.(n) <> [] then Some m.(n) else None in
	
	let () = List.iter (fun x -> if x <= n then m.(x) <- [x]) coins in
	g ();;

match (exchange (read_int()) [7;2]) with
	|None -> print_string "None"
	|Some l -> List.iter (fun x -> Printf.printf "%i;" x) l;;		 
		
	