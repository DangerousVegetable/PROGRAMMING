let rec serialise l a b = 
	let rec f num k = if k = 0 then [] else 
		(num land 1)::(f (num lsr 1) (k-1)) in
	l@(List.rev (f b a));;

let deserialise l a = 
	let rec f li num k =
		if k = 0 then (num,li) else	
			match li with
			|[] -> (num,[])
			|hd::tl -> f tl ((num lsl 1) + hd) (k-1) in
	
	f l 0 a;;

let split_to_bytes l = 
	let rec f li =
		match li with
		|[] -> []
		|_ -> let (num,tl) = deserialise li 8 in num::(f tl) in
	f l;;

let join_bytes l =	
	let rec f li =
		match li with
		|[] -> []
		|hd::tl -> (serialise [] 8 hd)@(f tl) in
	f l;;

let pl l =
	List.iter (fun x -> Printf.printf "%i; " x) l;;




let l = serialise((serialise (serialise (serialise [] 3 7) 4 11) 4 13)) 5 1;;
pl l;;
print_string "\n";;
let l = split_to_bytes l;;
pl l;;
print_string "\n";;
let l = join_bytes l;;
let (a,l) = deserialise l 3;;
let (b,l) = deserialise l 4;;
let (c,l) = deserialise l 4;;
let (d,l) = deserialise l 5;;
List.iter (fun x -> Printf.printf "%i; " x) (a::b::c::[d]);; 	



