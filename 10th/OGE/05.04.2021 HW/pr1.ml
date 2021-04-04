open String;;
open Array;;

let inp = open_in "text.txt";;

let find () = 
	let a = Array.make 256 false and
	b = Array.make 256 (-1) in

	let rec f m n pos = 
		try 
			let c = Char.code (input_char inp) in
			if not a.(c) then( 
				a.(c) <- true; 
				b.(c) <- pos;      
				f m (n+1) (pos+1))
			else ( 
				Array.iteri (fun i v -> if v < b.(c) then a.(i) <- false) b;
				b.(c) <- pos;
				f (max m n) (pos - b.(c)) (pos+1)) 
		with End_of_file -> max m n in
	
	f 0 0 0;;

print_int (find ());;
		