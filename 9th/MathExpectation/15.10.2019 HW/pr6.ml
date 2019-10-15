type prefix = Q of prefix*prefix|A of int;;
                              
exception Incorrect_arguments;;

let rec get_a p l = 
	match (p,l) with
	|(Q (p1,p2),a::b) -> if a = 0 then get_a p1 b else get_a p2 b
	|(A k, []) -> k
	|_ -> raise Incorrect_arguments;;

print_int (get_a (Q(A 1,Q(A 2,A 3))) [0;1]);;