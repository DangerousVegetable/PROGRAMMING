exception Found of (int*int) list;;

let solve n = 	
        let check x y l = if x >= 0 && x < n && y >= 0 && y < n then (not (List.exists (fun a -> a = (x,y)) l)) else false in

        let rec count col row l =
		let ans = ref 0 in
			if check (col-1) (row-2) l then ans:=!ans+1;
			if check (col+1) (row-2) l then ans:=!ans+1;
			if check (col+2) (row-1) l then ans:=!ans+1;
			if check (col+2) (row+1) l then ans:=!ans+1;
			if check (col+1) (row+2) l then ans:=!ans+1;
			if check (col-1) (row+2) l then ans:=!ans+1;
			if check (col-2) (row+1) l then ans:=!ans+1;
			if check (col-2) (row-1) l then ans:=!ans+1; !ans in
	

	let rec stp col row l i r = if i >= 8 then 
					List.sort (fun (a,_,_) (b,_,_) -> compare a b) r 
				    else 
			       	    	match i with
			                |0 -> if check (col-1) (row-2) l then stp col row l (i+1) ((count (col-1) (row-2) l,col-1,row-2)::r) else stp col row l (i+1) r
					|1 -> if check (col+1) (row-2) l then stp col row l (i+1) ((count (col+1) (row-2) l,col+1,row-2)::r) else stp col row l (i+1) r
					|2 -> if check (col+2) (row-1) l then stp col row l (i+1) ((count (col+2) (row-1) l,col+2,row-1)::r) else stp col row l (i+1) r
					|3 -> if check (col+2) (row+1) l then stp col row l (i+1) ((count (col+2) (row+1) l,col+2,row+1)::r) else stp col row l (i+1) r
					|4 -> if check (col+1) (row+2) l then stp col row l (i+1) ((count (col+1) (row+2) l,col+1,row+2)::r) else stp col row l (i+1) r
					|5 -> if check (col-1) (row+2) l then stp col row l (i+1) ((count (col-1) (row+2) l,col-1,row+2)::r) else stp col row l (i+1) r
					|6 -> if check (col-2) (row+1) l then stp col row l (i+1) ((count (col-2) (row+1) l,col-2,row+1)::r) else stp col row l (i+1) r
					|7 -> if check (col-2) (row-1) l then stp col row l (i+1) ((count (col-2) (row-1) l,col-2,row-1)::r) else stp col row l (i+1) r in
	

	let rec main step col row visited = 
		if step >= n*n-1 then raise (Found visited) else List.iter (fun (_,x,y) -> main (step+1) x y ((x,y)::visited)) (stp col row visited 0 []) in 
			
	try
	   main 0 0 0 [(0,0)]; None
	with Found l -> Some l;;

match solve (read_int()) with
	|None -> print_string "Impossible"
	|Some l -> print_string "Example: "; List.iter (fun (x,y) -> Printf.printf "(%i,%i)->" x y) (List.rev l);;

			   			
