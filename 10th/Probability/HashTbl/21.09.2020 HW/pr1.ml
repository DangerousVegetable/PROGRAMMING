let n = read_int();;

let l = List.map (fun x -> (x*x) mod 10) [0;1;2;3;4;5;6;7;8;9];;

(*let rec f l x n =
	match l with
	|[] -> []
	|a::tl -> if a = x then f tl x (n+1) else if n > 0 then (x,n)::(f tl a 1);;

let p = f l 0 0;;
*)
print_float ((float_of_int(List.fold_left (fun s x -> if x = n then s+1 else s) 0 l))/.(float_of_int (List.length l)));;



  
	

