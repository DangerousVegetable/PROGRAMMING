type tree = B of tree*tree| N of bool;;

type lambda = Var of string|App of lambda*lambda|Abs of string*lambda;;

let to_bin k n = 
	let rec f n k = 
		if n = 0 then [] else if (k mod 2) = 0 then (false::(f (n-1) (k/2))) else true::(f (n-1) (k/2)) in

	List.rev (f n k);;

let get_fun k n = to_bin k ((1 lsl n));;	

let lam k n = 
        let rec sep l m = 
		if m = 0 then ([],l) else match l with
					  |[] -> failwith "m is greater than list length"
					  |a::tl -> let (l1,l2) = sep tl (m-1) in (a::l1,l2) in
		
	let rec f n l =
		match l with
		|[] -> failwith ""
		|[false] -> Abs("x",Abs("y",Var "y"))
		|[true] -> Abs("x", Abs("y",Var "x"))
		|l -> let (left,right) = sep l ((List.length l)/2) in let left = f (n+1) left and right = f (n+1) right in if left = right then left else App(App (Var ("l"^(string_of_int n)),right), left) in

	let rec wrap m = 
		if m = n then f 0 (get_fun k n) else Abs ("l"^(string_of_int m),wrap (m+1)) in

	wrap 0;;

let rec l_to_string l = 
	match l with 
 	|Var s -> s
	|App (l1,l2) -> "("^(l_to_string l1)^" "^(l_to_string l2)^")"
	|Abs (s,l1) -> "\\"^s^"." ^ (l_to_string l1);; 

print_string (l_to_string (lam 2 4));; 
