open Array;;

let num_of_edges a = fold_left (fun t x -> t + fold_left (fun u a -> if a then u+1 else u) 0 x) 0 a;;

let a = [|[|false;false;false;false;false|];
	  [|false;false;true;false;false|];
	  [|false;false;false;true;false|];
	  [|false;false;false;false;true|];
	  [|false;false;false;false;false|];
	|];;

print_int (num_of_edges a);;	