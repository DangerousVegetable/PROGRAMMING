open String;;

let check s = 
	let rec f n =
		if n >= length s then (length s) else 
		match s.[n] with
		|'(' -> let nn = f(n+1) in if s.[nn] = ')' then let newn = f (nn+1) in newn else n 
		|')' -> n 
		|_ -> n(*failwith "Unexpected symbol"*) in
f 0;;

(*let s = read_line();;

let n = check (s^" ");;                            

print_int n;;  *)      


let mexp n = 
	let rec f s k = 
		if k = 2*n then float_of_int(check (s^" ")) else ((f (s^"(") (k+1))+.(f (s^")") (k+1)))/.2. in
f "" 0;;

Printf.printf "M: %f" (mexp (read_int()));; 