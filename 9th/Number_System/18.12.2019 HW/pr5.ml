let from_list l n = 	
	let (k,_) = List.fold_left (fun (r,i) x -> (r+x*(int_of_float ( (float_of_int n)**(float_of_int i) ) ),i+1) ) (0,0) l in k;;

let from_3_ l = from_list l 3;;

print_int (from_3_ [-1;0;0;1]);; (*== 26*)