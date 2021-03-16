open String;;

type regexp = Concat of regexp*regexp|Alt of regexp*regexp| Char of char|Empty|Range of char*char;;

let parse s = 
	let rec parse_s r n = 
		if n >= length s then (r,n) else let c = s.[n] in
			match c with
			|'(' -> let (exp,n) = parse_s Empty (n+1) in 
							if s.[n] = ')' then 
								if exp = Empty then parse_s r (n+1)
								else if r = Empty then parse_s exp (n+1) else parse_s (Concat(r,exp)) (n+1) 
							else 
								failwith ("Expected ): "^(string_of_int n))
			|'[' -> let (exp,n) = parse_t Empty (n+1) in
							if s.[n] = ']' then
								if exp = Empty then parse_s r (n+1)
								else if r = Empty then parse_s exp (n+1) else parse_s (Concat(r,exp)) (n+1) 
							else 
								failwith ("Expected ]: "^(string_of_int n))
			|'|' -> let (exp,n) = parse_s Empty (n+1) in parse_s (Alt(r,exp)) n
			|')' -> (r,n)
			|c ->  if r = Empty then parse_s (Char c) (n+1) else if r = Empty then parse_s (Char c) (n+1) else parse_s (Concat(r,Char c)) (n+1)

								
	and parse_t r n =
		let next_ n = if n >= length s then false else s.[n] = '-' in

		if n>=length s then (Empty,n) else let c = s.[n] in
			match c with
			|'(' -> let (exp,n) = parse_s Empty (n+1) in 
							if s.[n] = ')' then 
								if exp = Empty then parse_t r (n+1)  
								else if r = Empty then parse_t (exp) (n+1) else parse_t (Concat(r,exp)) (n+1) 
							else failwith ("Expected ): "^(string_of_int n))
			|'[' -> failwith ("Unexpected \'[\' insinde [...]: "^(string_of_int n))
			|']' -> (r,n)
			|c -> 	if next_ (n+1) then let c2 = s.[n+2] in if r = Empty then parse_t (Range(c,c2)) (n+3) else parse_t (Concat(r,Range(c,c2))) (n+3) 
				else if r = Empty then parse_t (Char c) (n+1) else parse_t (Concat(r, Char c)) (n+1) in 
			 
	let (p,n) = parse_s Empty 0 in
		if n >= length s then p else failwith "Unappropriate string format!";; 

let rec string_of_reg r = 
	match r with
	|Concat (r1,r2) -> (string_of_reg r1)^"+"^(string_of_reg r2)
	|Alt (r1,r2) -> "("^(string_of_reg r1)^"|?|"^(string_of_reg r2)^")" 
	|Char c -> make 1 c
	|Empty -> ""
	|Range (c1,c2) -> "["^(make 1 c1)^"<->"^(make 1 c2)^"]";;

(*let str = "(1|2|3)[adkf0-9a-f]abcd[0-1](1|)";;

print_string (string_of_reg (parse str));;     *)             
			
	 	