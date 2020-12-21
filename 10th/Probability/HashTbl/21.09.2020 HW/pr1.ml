let n = read_int();;
let m = read_int();;
let l = List.map (fun x -> (x*x) mod 10) (List.init (int_of_float(sqrt (float_of_int n))) (fun x -> x+1));;

(*let rec f l x n =
	match l with
	|[] -> []
	|a::tl -> if a = x then f tl x (n+1) else if n > 0 then (x,n)::(f tl a 1);;

let p = f l 0 0;;
*)
print_float ((float_of_int(List.fold_left (fun s x -> if x = m then s+1 else s) 0 l))/.(float_of_int (List.length l)));;



  
	

