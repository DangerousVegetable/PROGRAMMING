open List;;

let rec qsort f l =
	if length l = 1 then 
		l 
	else 
		match l with
		|[] -> []
		|a::tl -> (qsort f (filter (fun x -> f x a) l))@(filter (fun x -> x = a) l)@(qsort f (filter (fun x -> f a x) l));;

let rec findmostcommon l = 
	let rec (<$) s1 s2 =                                    
			if String.length s1 = 0 || String.length s2 = 0 then 
				false 
			else 
				if s1.[0] = s2.[0] then 
					(String.sub s1 1 (String.length s1 - 1)) <$ ((String.sub s2 1 (String.length s2 - 1)))
			    	else s1.[0] < s2.[0] in 

	let rec merge l1 l2 = 
		match (l1,l2) with
		|([],_) -> l2
		|(_,[]) -> l1
		|((s1,n1)::tl1,(s2,n2)::tl2) -> if s1 = s2 then (s1,n1+n2)::(merge tl1 tl2) else if s1 <$ s2 then (s1,n1)::(merge tl1 l2) else (s2,n2)::(merge l1 tl2) in

	let rec f l = 
		match l with
		|[] -> []
		|a::[] -> [a]
		|a::b::tl -> f((merge a b)::(f tl)) in 

	match l with
	|[] -> failwith "Empty input"
	|_ -> hd (qsort (fun (a,n) (b,m) -> if n = m then a <$ b else n > m) (flatten (f (map (fun x -> [(x,1)]) l))));;

let (s,n) = findmostcommon ["abab";"b";"cafaf";"abab";"fl";"cafaf";"cafaf";"b"];; 
Printf.printf "%n: %s" n s;; 
	